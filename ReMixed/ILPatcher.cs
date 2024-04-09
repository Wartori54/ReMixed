#define VERIFY_ARGS

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Mono.Cecil;
using Mono.Cecil.Cil;
using ReMixed.Positioning;
using BindingFlags = System.Reflection.BindingFlags;
using Cursor = ReMixed.PatchPlatform.Cursor;
using ILLabel = ReMixed.PatchPlatform.ILLabel;
using MethodBody = Mono.Cecil.Cil.MethodBody;

namespace ReMixed;

public static class ILPatcher {

    /// <summary>
    /// Replaces a const.
    /// </summary>
    /// <param name="cursor">The cursor.</param>
    /// <param name="target">The value to find.</param>
    /// <param name="value">The new value.</param>
    /// <typeparam name="T">The type of the constant to target.</typeparam>
    public static void ReplaceNextConst<T>(Cursor cursor, T target, T value) {
        FindNextConst(cursor, target);
        
        ReplaceLdc(cursor.Next!, value);
    }

    /// <summary>
    /// Replaces a const, with a delegate
    /// </summary>
    /// <param name="cursor">The cursor.</param>
    /// <param name="target">The value to find.</param>
    /// <param name="value">The new not-so constant value.</param>
    /// <typeparam name="T">The type of the constant to target.</typeparam>
    public static void ReplaceNextConst<T>(Cursor cursor, T target, Func<T> value) {
        FindNextConst(cursor, target);
        
        cursor.RemoveNext();
        
        cursor.EmitDelegate(value);
    }

    /// <summary>
    /// Moves the cursor after the specified const.
    /// </summary>
    /// <param name="cursor">The cursor.</param>
    /// <param name="target">The value to find.</param>
    /// <typeparam name="T">The type of the constant to target.</typeparam>
    /// <exception cref="InvalidOperationException"></exception>
    /// <exception cref="ArgumentOutOfRangeException"></exception>
    public static void FindNextConst<T>(Cursor cursor, T target) {
        Predicate<Instruction> selector = target switch {
            float v => i => i.MatchLdcR4(v),
            double v => i => i.MatchLdcR8(v),
            int v => i => i.MatchLdcI4(v),
            long v => i => i.MatchLdcI8(v),
            _ => throw new InvalidOperationException("Invalid constant type!")
        };

        if (!cursor.TryGotoNext(selector)) {
            throw new ArgumentOutOfRangeException(nameof(cursor), "Cannot find target!");
        }
    }

    private static void ReplaceLdc<T>(Instruction instr, T value) {
        switch (value) {
            case double d:
                instr.OpCode = OpCodes.Ldc_R8;
                instr.Operand = d;
                break;
            case float f:
                instr.OpCode = OpCodes.Ldc_R4;
                instr.Operand = f;
                break;
            case long l:
                instr.OpCode = OpCodes.Ldc_I8;
                instr.Operand = l;
                break;
            case int i: // ints are fun: theres 12 ways
                switch (i) { // Switch in switch :D
                    case >= -1 and <= 8: // Use faster aliases for small values
                        instr.OpCode = i switch {
                            0 => OpCodes.Ldc_I4_0,
                            1 => OpCodes.Ldc_I4_1,
                            2 => OpCodes.Ldc_I4_2,
                            3 => OpCodes.Ldc_I4_3,
                            4 => OpCodes.Ldc_I4_4,
                            5 => OpCodes.Ldc_I4_5,
                            6 => OpCodes.Ldc_I4_6,
                            7 => OpCodes.Ldc_I4_7,
                            8 => OpCodes.Ldc_I4_8,
                            -1 => OpCodes.Ldc_I4_M1,
                            _ => throw new ArgumentOutOfRangeException($"{nameof(i)}"),
                        };
                        instr.Operand = null;
                        break;
                    case >= -128 and <= 127: // Use short form for bytes
                        instr.OpCode = OpCodes.Ldc_I4_S;
                        instr.Operand = i;
                        break;
                    default: // Default to default way
                        instr.OpCode = OpCodes.Ldc_I4;
                        instr.Operand = i;
                        break;
                }

                break;
        }
    }

    /// <summary>
    /// Injects a call before any method.
    /// </summary>
    /// <param name="cursor">A cursor, its position will be overriden.</param>
    /// <param name="target">The target inject location.</param>
    /// <param name="injection">The payload.</param>
    public static void InjectCallAt(Cursor cursor, InjectLocation target, MethodBase injection) {
        // Reset cursor
        cursor.GotoFirst();
        // Find target
        int popCount = GoToCall(cursor, target);
        Instruction? prevInstr = cursor.Previous;
        // Do the injection
        InternalInjectCallAt(cursor, popCount, injection, target.Cancellable);
        // Register it
        int startInjIdx = prevInstr == null ? 0 : cursor.Method.Body.Instructions.IndexOf(prevInstr.Next);
        int endInjIdx = cursor.Next == null ? cursor.Method.Body.Instructions.Count : cursor.Method.Body.Instructions.IndexOf(cursor.Next);
        InjectionTracker.InjectionData data = cursor.Platform.InjectionTracker.RegisterInjection(cursor.Method.Body,
            startInjIdx,
            endInjIdx - startInjIdx,
            !InjectLocation.ShiftReplacesInstr(target.ShiftBy));
        
        if (InjectLocation.ShiftReplacesInstr(target.ShiftBy)) {
            RetargetJumps(cursor.Platform, prevInstr?.Next, cursor.Next, data);
        }
    }
    
    /// <summary>
    /// Injects a call before any method.
    /// </summary>
    /// <param name="cursor">A cursor, its position will be overriden.</param>
    /// <param name="target">The target inject location.</param>
    /// <param name="injection">The payload.</param>
    /// <typeparam name="T">The return type of the method.</typeparam>
    /// <exception cref="InvalidOperationException">If something goes wrong.</exception>
    public static void InjectCallAt<T>(Cursor cursor, InjectLocation target, MethodBase injection) {
        // Reset cursor
        cursor.GotoFirst();
        // Find target
        int popCount = GoToCall(cursor, target); // Simple enough, right?
        Instruction? prevInstr = cursor.Previous;
        // Do the injection
        if (target.Cancellable)
            InternalInjectCallAt<T>(cursor, popCount, injection);
        else
            InternalInjectCallAt(cursor, popCount, injection, false);
        // Register it
        int startInjIdx = prevInstr == null ? 0 : cursor.Method.Body.Instructions.IndexOf(prevInstr.Next);
        int endInjIdx = cursor.Next == null ? cursor.Method.Body.Instructions.Count : cursor.Method.Body.Instructions.IndexOf(cursor.Next);
        InjectionTracker.InjectionData data = cursor.Platform.InjectionTracker.RegisterInjection(cursor.Method.Body,
            startInjIdx,
            endInjIdx - startInjIdx,
            !InjectLocation.ShiftReplacesInstr(target.ShiftBy));
        
        if (InjectLocation.ShiftReplacesInstr(target.ShiftBy)) {
            RetargetJumps(cursor.Platform, prevInstr?.Next, cursor.Next, data);
        }
        
    }
    
    // Emits:
    // ldc.i4 {0, 1}
    // newobj CallbackInfo::.ctor(int)
    // dup
    // call {void delegate(CallbackInfo)}
    // call bool CallbackInfo::IsCanceled()
    // brfalse {NEXT ORIG METHOD INSTRUCTION}
    // pop...pop
    // ret
    // It can also omit the canceling part to just call a method.
    private static void InternalInjectCallAt(Cursor cursor, int popCount, MethodBase injection, bool cancellable) {
        // Cursor is assumed to be at the correct position
        
        EmitCallbackInfo(cursor, cancellable); // Emit the CI
        
        // This is only required if its cancellable
        if (cancellable)
            cursor.EmitDup(); // Will be used by the `IsCanceled` in case its marked as such

        EmitInjectionMethod(injection, cursor); // the injection

        if (!cancellable) return;
        
        cursor.EmitCall(typeof(CallbackInfo).GetMethod(nameof(CallbackInfo.IsCanceled), BindingFlags.Instance | BindingFlags.Public)
                                    ?? throw new InvalidOperationException()); // is cancel
        
        ILLabel continueLabel = cursor.GetLabel(); // The next instruction is the next orig instruction so get a label to it
        cursor.EmitBrFalse(continueLabel); // and point the brfalse to it
        
        // Emit enough pops to empty the stack
        for (int i = 0; i < popCount; i++) {
            cursor.EmitPop();
        }

        cursor.EmitRet(); // Done
    }
    
    // Emits:
    // newobj CallbackInfoRet<T>::.ctor()
    // dup
    // dup
    // stloc cir
    // call {void Delegate(CallbackInfoRet<T>)}
    // call bool CallbackInfoRet<T>::IsCanceled()
    // brfalse {NEXT ORIG METHOD INSTRUCTION}
    // pop...pop # as many as necessary to clear the stack
    // ldloc cir
    // IL_ret_routine: call T CallbackInfoRet<T>::GetRet
    // ret
    private static void InternalInjectCallAt<T>(Cursor cursor, int popCount, MethodBase injection) {
        // Cursor is assumed to be at the correct position
        
        EmitCallbackInfoRet<T>(cursor); // add the newobj

        cursor.EmitDup(); // will be used by IsCancelled
        cursor.EmitDup(); // will be stored in a local variable
        VariableDefinition cirLoc = cursor.CreateLocal(typeof(CallbackInfoRet<T>));
        cursor.EmitStLoc(cirLoc);
        
        EmitInjectionMethod(injection, cursor); // the delegate

        cursor.EmitCall(typeof(CallbackInfo).GetMethod(nameof(CallbackInfo.IsCanceled), BindingFlags.Instance | BindingFlags.Public)
                            ?? throw new InvalidOperationException()); // is cancel

        // Label to continue execution normally
        ILLabel continueLabel = cursor.GetLabel();
        cursor.EmitBrFalse(continueLabel); // Targets the next instruction since it is the next method's instruction
        
        // Emit all the pops required to empty the stack at this point
        for (int i = 0; i < popCount; i++) {
            cursor.EmitPop();
        }

        // Get the cir again
        cursor.EmitLdLoc(cirLoc);
        
        cursor.EmitCall(typeof(CallbackInfoRet<T>).GetMethod(nameof(CallbackInfoRet<T>.GetRet), BindingFlags.Instance | BindingFlags.Public) 
                        ?? throw new InvalidOperationException()); // get ret

        cursor.EmitRet(); // the return
    }

    /// <summary>
    /// Moves the cursor to the specified place in the InjectLocation.
    /// </summary>
    /// <param name="cursor">The cursor to move.</param>
    /// <param name="target">The InjectLocation to follow.</param>
    /// <returns>An integer containing the amount of elements that have to be popped to cancel a call.</returns>
    /// <exception cref="ArgumentOutOfRangeException"></exception>
    private static int GoToCall(Cursor cursor, InjectLocation target) {
        for (int i = 0; i < target.Idx + 1; i++) {
            i = cursor.Platform.InjectionTracker.NextFreeIndex(i);
            if (!cursor.TryGotoNext(delegate(Instruction instruction) {
                    // we cannot use the predicate directly since it could match injected instrs
                    if (target.Target.Predicate(instruction)) {
                        int idx = cursor.Method.Body.Instructions.IndexOf(instruction);
                        return !cursor.Platform.InjectionTracker.IsInInjection(idx);
                    }

                    return false;
                }))
                throw new ArgumentOutOfRangeException($"{nameof(target)}",
                                    "No instruction matches the given criteria.");
        }

        return target.Target.HandleShift(cursor, target.ShiftBy);
    }

    private static void EmitCallbackInfo(Cursor cursor, bool returnable) {
        cursor.EmitLdcI4(returnable ? 1 : 0);
        cursor.EmitNewobj(typeof(CallbackInfo).GetConstructor([typeof(bool)]) 
                          ?? throw new InvalidOperationException());
    }

    private static void EmitCallbackInfoRet<T>(Cursor cursor) {
        cursor.EmitNewobj(typeof(CallbackInfoRet<T>).GetConstructor([]) 
                          ?? throw new InvalidOperationException());
    }

    // Emits the injection, analyzing its parameters: it may capture the instance, the target method args, both or nothing
    private static void EmitInjectionMethod(MethodBase injection, Cursor cursor) {
        MethodReference methodReference = cursor.Method.Module.ImportReference(injection);
        MethodDefinition origMethod = cursor.Platform.GetRealMethod();
        // See `GetRealMethod` docs
        (bool capturesInstance, bool capturesArguments) = AnalyzeMethodReference(methodReference, origMethod);
        
        if (capturesInstance)
            cursor.EmitLdarg0();
        // Dont use indexes here since there may be a lot of args and it simplifies the logic on instance methods
        if (capturesArguments) {
            for (int i = 0; i < origMethod.Parameters.Count; i++) {
                cursor.EmitLdarg(i);
            }
        }

        cursor.EmitCall(injection);
    }

    // Verifies an injection method contains the correct arguments for its task
    private static (bool capturesInstance, bool capturesArguments) AnalyzeMethodReference(MethodReference methodReference, 
        MethodDefinition location) {
        if (methodReference.HasGenericParameters) { // TO DO
            throw new NotImplementedException();
        }

        if (methodReference.HasThis) throw new InvalidOperationException(); // Cant inject instance methods
        
        if (methodReference.Parameters.Count == 0) throw new InvalidOperationException(); // TODO: Doc errors
        
        CIReference ??= location.Module.ImportReference(typeof(CallbackInfo));
        CIRReference ??= location.Module.ImportReference(typeof(CallbackInfoRet<>));
        
        // Must start with a callback info of some type
        if (!TypeReferenceEqual(methodReference.Parameters[0].ParameterType, CIReference) && 
            TypeReferenceEqual(methodReference.Parameters[0].ParameterType, CIRReference))
            throw new InvalidOperationException();
        // Can be a ci alone
        if (methodReference.Parameters.Count == 1) // Single arg, captures nothing
            return (false, false);

        // 3 options remain: ci & instance, ci & args, ci & instance & args
        bool withInstance = false;
        if (TypeReferenceEqual(methodReference.Parameters[1].ParameterType, location.DeclaringType)) { // ci  & instance (& args)
            if (methodReference.Parameters.Count == 2) // Captures ci and its instance
                return (true, false);
            
            withInstance = true; // Has instance and args
        }
        // Quick amount check, account of the ci as first argument too
        if (methodReference.Parameters.Count != location.Parameters.Count + (withInstance ? 2 : 1))
            throw new InvalidOperationException();
        
#if VERIFY_ARGS // This is a bit expensive, may be disabled if performance is a concern
        for (int i = withInstance ? 2 : 1; i < methodReference.Parameters.Count; i++) {
            if (!TypeReferenceEqual(methodReference.Parameters[i].ParameterType, location.Parameters[i - (withInstance ? 2 : 1)].ParameterType)) {
                throw new InvalidOperationException();
            }
        }
#endif
        
        return (withInstance, true);
    }

    private static void RetargetJumps(PatchPlatform platform,
        Instruction? injStart,
        Instruction? bodyContinue,
        InjectionTracker.InjectionData injData) {
        if (injStart == null || bodyContinue == null) return;
        
        // Make sure to obtain the orig index for the StAnalysis
        int idxInjStart = platform.PlatformCursor.Method.Body.Instructions.IndexOf(injStart);
        int idxInjEnd = platform.PlatformCursor.Method.Body.Instructions.IndexOf(bodyContinue);
        int origIndex = platform.InjectionTracker.CalculateOrigIndex(idxInjEnd);
        // TODO DEBUG REMOVE THIS
        if (origIndex != platform.InjectionTracker.CalculateOrigIndex(idxInjStart))
            throw new InvalidOperationException();

        // TODO: check if theres an injection before this
        // Replace all branches with this instr, in case it has not been retargeted
        bool hasBeenRetargeted = platform.InjectionTracker.Retargeted.GetValueOrDefault(origIndex, false);
        List<int> branches = hasBeenRetargeted ? [] : platform.StAnalysis.Branches.GetValueOrDefault(origIndex, []);
        platform.InjectionTracker.Retargeted[origIndex] = true;
        foreach (int branch in branches) {
            int inModifiedIdx = platform.InjectionTracker.CalculateModifiedIndex(branch);
            MethodBody body = platform.PlatformCursor.Method.Body;
            Instruction targetInstr = body.Instructions[inModifiedIdx];
            object standardizedObject =
                platform.StAnalysis.OperandConverter?.Invoke(targetInstr.Operand, body, body.Instructions) ??
                targetInstr.Operand;
            if (standardizedObject is Instruction) {
                targetInstr.Operand = injStart;
            } else if (standardizedObject is Instruction[] jmps) {
                for (int i = 0; i < jmps.Length; i++) {
                    if (jmps[i] == bodyContinue) {
                        jmps[i] = injStart;
                    }
                }
            } else {
                throw new InvalidOperationException("BUG! Jump from a non-jumpable instruction!");
            }
        }

        // Check if there are injections before this
        if (injData.StartIdx != idxInjStart) { // If so retarget its branches to us instead of the body
            MethodBody body = platform.PlatformCursor.Method.Body;
            for (int i = idxInjStart - 1; i >= injData.StartIdx; i--) { // TODO: make this decently fast
                Instruction targetInstr = platform.PlatformCursor.Method.Body.Instructions[i];
                object standardizedOp = platform.StAnalysis.OperandConverter?.Invoke(targetInstr.Operand, body, body.Instructions) ??
                                                        targetInstr.Operand;
                if (standardizedOp is Instruction instr) { // Switches in injections are not supported yet
                    if (instr == bodyContinue) {
                        platform.PlatformCursor.Method.Body.Instructions[i].Operand = injStart;
                    }
                }
            }
        }
    }

    private static TypeReference? CIReference;
    private static TypeReference? CIRReference;

    // I know this is really bad, and hopefully cecil will add a way to compare TypeReferences, but for the meantime, this is it.
    private static readonly Func<TypeReference, TypeReference, bool> TypeReferenceEqual =
        typeof(MetadataResolver).GetMethod("AreSame", BindingFlags.Static | BindingFlags.NonPublic,
            [typeof(TypeReference), typeof(TypeReference)])!.CreateDelegate<Func<TypeReference, TypeReference, bool>>();

    // public class MethodTarget {
    //     public InjectTarget InjectTarget { get; }
    //     public bool Cancellable { get; }
    //     public  Shift ShiftBy { get; }
    //     public  int Idx { get; }
    //     public bool CaptureInstance { get; }
    //     public bool CaptureArgs { get; }
    //
    //     public MethodTarget(InjectTarget injectTarget, bool cancellable, Shift shift = Shift.BeforeArguments,
    //         int idx = 0, bool captureInstance = true, bool captureArgs = true) {
    //         InjectTarget = injectTarget;
    //         Cancellable = cancellable;
    //         ShiftBy = shift;
    //         Idx = idx;
    //         CaptureInstance = captureInstance;
    //         CaptureArgs = captureArgs;
    //     }
    //
    //     
    // }

    // public class InjectLocation {
    //     private readonly string typeS;
    //     private readonly LocalFunctionStatementSyntax? functionSyntax;
    //     private MethodReference? resolvedMethod = null;
    //     private SpecialInjectLocation injectLocationName;
    //     public MethodReference? ResolvedMethod => resolvedMethod;
    //
    //     public SpecialInjectLocation InjectLocationName {
    //         get => injectLocationName;
    //         private set => injectLocationName = value;
    //     }
    //
    //     // public InjectLocation(Type type, string name) {
    //     //     this.type = type;
    //     //     this.name = name;
    //     //     this.typeS = "";
    //     // }
    //
    //     public InjectLocation(string target, bool disableSpecial = false) {
    //         if (ParseInjectLocation(target, out injectLocationName)) {
    //             typeS = "";
    //             functionSyntax = null;
    //         } else { // Parse the `target` into type, name and arguments
    //             string[] parts = target.Split(';');
    //             if (parts.Length != 2)
    //                 throw new NotSupportedException(
    //                     "Parameter must contain exactly 1 semicolon between the type and the method decl");
    //             typeS = parts[0];
    //             
    //             //https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/types
    //             SyntaxTree tree = CSharpSyntaxTree.ParseText(parts[1]);
    //             LocalFunctionStatementSyntax? localFunctionStatementSyntax = (tree.GetRoot().ChildNodes().First().ChildNodes().First() as LocalFunctionStatementSyntax);
    //             functionSyntax = localFunctionStatementSyntax ?? throw new NotSupportedException("Cannot parse method target!");
    //             // return;
    //             // ReadOnlySpan<char> targetSpan = target.AsSpan();
    //             // int typeStart = targetSpan.IndexOf(' ');
    //             // if (typeStart == -1)
    //             //     throw new ArgumentException("Cannot find return value!", nameof(target));
    //             // returnValue = Typing.FullTypeBasicTypes(targetSpan[..typeStart].ToString());
    //             //
    //             // int parenthesisStart = targetSpan.IndexOf('(');
    //             // int methodNameStart = targetSpan[typeStart..parenthesisStart].LastIndexOf('.');
    //             // name = targetSpan[(methodNameStart + 1)..parenthesisStart].ToString();
    //             // if (name[^1] == '>')
    //             //     throw new NotImplementedException("Injecting into generics is not yet implemented!");
    //             //
    //             // typeS = targetSpan[(typeStart + 1)..methodNameStart].ToString();
    //             //
    //             //
    //             // return;
    //             // // TODO: Hooking generics?
    //             // // TODO: Improve generic parameter parsing a LOT
    //             // int sepIdx = target.IndexOf("::", StringComparison.Ordinal);
    //             // if (sepIdx == -1) throw new ArgumentException(null, $"{nameof(target)}");
    //             // // Obtain type and name
    //             // ReadOnlySpan<char> typeString = target.AsSpan()[..sepIdx];
    //             // ReadOnlySpan<char> nameString = target.AsSpan()[(sepIdx + 2)..]; // Theres a *double* colon separator
    //             // typeS = typeString.ToString();
    //             // int startParenthesis = nameString.IndexOf("(");
    //             // name = nameString[..startParenthesis].ToString();
    //             //
    //             // // Parse arguments
    //             // int currArgStart = startParenthesis + 1;
    //             // int genericLayers = 0; // Parameters with generic types may contain ','
    //             // for (int i = startParenthesis+1; i < nameString.Length; i++) {
    //             //     switch (nameString[i]) {
    //             //         case ')': // End or next arg
    //             //         case ',' when genericLayers == 0: {
    //             //             if (currArgStart == i) break; // 0-sized arg (likely to be parameterless method)
    //             //             ReadOnlySpan<char> arg = nameString[currArgStart..i];
    //             //             arg = arg.Trim();
    //             //             // TODO: Flexibilize generic types on params
    //             //             methodParams.Add(Typing.FullTypeBasicTypes(new string(arg))); // Allow for easier native types
    //             //             currArgStart = i + 1;
    //             //             break;
    //             //         }
    //             //         case '<':
    //             //             genericLayers++;
    //             //             break;
    //             //         case '>':
    //             //             genericLayers--;
    //             //             break;
    //             //     }
    //             // }
    //             //
    //             // if (genericLayers != 0) 
    //             //     throw new ArgumentException($"{nameof(target)} is has invalid generic types!");
    //         }
    //     }
    //
    //     public bool Predicate(Instruction instruction) {
    //         return instruction.MatchCall(functionSyntax, typeS, out resolvedMethod);
    //         // return instruction.MatchCall(typeS, name, methodParams, out resolvedMethod);
    //     }
    //
    //
    //     private static bool ParseInjectLocation(string name, out SpecialInjectLocation ret) {
    //         switch (name) {
    //             case "HEAD":
    //                 ret = SpecialInjectLocation.Head;
    //                 return true;
    //             case "TAIL":
    //                 ret = SpecialInjectLocation.Tail;
    //                 return true;
    //             case "RETURN":
    //                 ret = SpecialInjectLocation.Return;
    //                 return true;
    //             default:
    //                 ret = SpecialInjectLocation.None;
    //                 return false;
    //         }
    //     }
    //
    //     public enum SpecialInjectLocation {
    //         None,
    //         Head,
    //         Return,
    //         Tail,
    //     }
    // }

    public class CallbackInfo {
        private readonly bool cancelable;
        private bool cancelled;

        public CallbackInfo(bool cancelable = false) {
            this.cancelable = cancelable;
            cancelled = false;
        }

        public virtual void Cancel() {
            if (!cancelable)
                throw new Exception("Cannot cancel non-cancellable injection!"); // TODO: exceptions
            cancelled = true;
        }

        public bool IsCanceled() {
            return cancelled;
        }
    }

    public class CallbackInfoRet<T> : CallbackInfo {
        private T? retValue;

        // This always will cancel
        public CallbackInfoRet() : base(true) {
        }

        public override void Cancel() {
            if (retValue == null)
                throw new Exception($"Cannot cancel {nameof(CallbackInfoRet<T>)} without setting a return value!");
            base.Cancel();
        }

        public void Cancel(T value) {
            SetReturnValue(value);
            base.Cancel();
        }

        public void SetReturnValue(T value) {
            retValue = value;
        }

        public T GetRet() {
            if (retValue == null) throw new InvalidOperationException("Cannot return without a return value!");
            return retValue;
        }
    }
}
