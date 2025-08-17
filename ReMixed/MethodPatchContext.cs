using System;
using System.Collections.Generic;
using System.Reflection;
using System.Text;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Collections.Generic;
using MonoMod.Cil;
using ReMixed.PlatformImpls;
using MethodBody = Mono.Cecil.Cil.MethodBody;

namespace ReMixed;

public abstract class MethodPatchContext {
    public PatchPlatform Platform { get; }

    private readonly MethodDefinition method;
    public MethodDefinition Method => method;
    
    private StackAnalysis? analysis;
    public StackAnalysis StAnalysis => analysis ??= new StackAnalysis(method, GetStAnalysisConverter());

    private InjectionTracker? injectionTracker;
    // Lazy loaded to remove stateful calls from the ctor
    public InjectionTracker InjectionTracker =>
        injectionTracker ??= new InjectionTracker(GetRealMethod().Body, StAnalysis);

    protected MethodPatchContext(MethodDefinition patchingMethod, PatchPlatform platform) {
        method = patchingMethod;
        Platform = platform;
    }

    protected abstract Func<object, MethodBody, Collection<Instruction>, object>? GetStAnalysisConverter();

    /// <summary>
    /// Obtains the target method's MethodDefinition, this is necessary since in some context the modified and the
    /// target method may not be the same.
    /// </summary>
    /// <returns></returns>
    public abstract MethodDefinition GetRealMethod();

    public abstract MethodReference ImportMethod(MethodBase method);

    public LegCursor GetCursor() {
        return new LegCursor(this, method.Body.Instructions, new MonoModPatchableMethodDefinition.MethodBodyDMDCombiner(GetRealMethod().Body, method.Body));
    }

    public class LegCursor : Cursor {
        public MethodDefinition Method => Context.Method;
        public readonly MethodPatchContext Context;
        public LegCursor(MethodPatchContext context, Collection<Instruction> cursorInstructions, CollectionILProcessor.IBodyDataProvider bodyDataProvider) : base(cursorInstructions, bodyDataProvider) {
            Context = context;
        }
    }

    // Hint: RO Cursor
    public class Positioner {
        public int Index { get; private set; } = 0;
        protected readonly Collection<Instruction> Instructions;
        
        public Instruction? Next => Index < Instructions.Count ? Instructions[Index] : null;
        public Instruction? Previous => Index > 0 ? Instructions[Index - 1] : null;

        public Positioner(Collection<Instruction> positionerInstructions) {
            Instructions = positionerInstructions;
        }
        
        public bool TryGotoNext(params Predicate<Instruction>[] predicates) {
            // if (predicates.Length == 0) return false;
            for (int i = Index; i + predicates.Length <= Instructions.Count; i++) {
                for (int j = 0; j < predicates.Length; j++) {
                    if (!predicates[j](Instructions[i + j])) goto skip;
                }
                Index = i;
                return true;
                skip:
                ;
            }
            return false;
        }
        
        public Positioner GotoNext(params Predicate<Instruction>[] predicates) {
            if (!TryGotoNext(predicates)) throw new InvalidOperationException(); // TODO: errors
            return this;
        }

        public Positioner GotoFirst() {
            Index = 0;
            return this;
        }

        public Positioner GotoLast() {
            Index = Instructions.Count;
            return this;
        }

        public Positioner MoveIndex(int move) {
            Index = Math.Clamp(Index + move, 0, Instructions.Count);
            return this;
        }

        public Positioner GotoInstr(Instruction target) {
            Index = Instructions.IndexOf(target);
            return this;
        }

        public virtual Positioner Clone() {
            return new Positioner(Instructions) {
                Index = Index,
            };
        }
    }
    
    public class Cursor : Positioner {
        private readonly CollectionILProcessor.IBodyDataProvider body;

        private readonly CollectionILProcessor il;

        public Cursor(Collection<Instruction> cursorInstructions, CollectionILProcessor.IBodyDataProvider bodyDataProvider) : base(cursorInstructions){
            body = bodyDataProvider;
            il = new CollectionILProcessor(cursorInstructions, body);
        }

        public Cursor RemoveNext() {
            il.RemoveAt(Index);
            return this;
        }

        public Cursor RemoveAll() {
            il.Clear();
            return this;
        }

        public void Emit(Instruction instruction) {
            if (il.Count == 0) {
                il.Append(instruction);
                return;
            }
            if (Index == 0)
                il.InsertBefore(Instructions[0], instruction); // Why cant you use indecies with this :(
            else
                il.InsertAfter(Index-1, instruction);
            MoveIndex(1);
        }

        public Cursor EmitDup() {
            Emit(il.Create(OpCodes.Dup));
            return this;
        }
        public Cursor EmitLdcI4(int value) {
            // TODO: Use proper ldcs
            Emit(il.Create(OpCodes.Ldc_I4, value));
            return this;
        }

        public Cursor EmitLdcI8(long value) {
            Emit(il.Create(OpCodes.Ldc_I8, value));
            return this;
        }
        
        public Cursor EmitLdcR4(float value) {
            Emit(il.Create(OpCodes.Ldc_R4, value));
            return this;
        }

        public Cursor EmitLdcR8(double value) {
            Emit(il.Create(OpCodes.Ldc_R8, value));
            return this;
        }

        public Cursor EmitLdLoc(VariableDefinition reference) {
            Emit(il.Create(OpCodes.Ldloc, reference));
            return this;
        }
        public Cursor EmitStLoc(VariableDefinition reference) {
            Emit(il.Create(OpCodes.Stloc, reference));
            return this;
        }
        public Cursor EmitLdarg0() {
            Emit(il.Create(OpCodes.Ldarg_0));
            return this;
        }
        /// <summary>
        /// Emits the load instruction for the i-th argument.
        /// WARNING: This skips over the instance in case its available.
        /// </summary>
        public Cursor EmitLdarg(int index) {
            // MonoMod change the signature of methods, must use the original here
            if (body.HasThis)
                index++;
            Emit(il.Create(OpCodes.Ldarg, index));
            return this;
        }
        public Cursor EmitCall(MethodReference target) {
            Emit(il.Create(OpCodes.Call, target));
            return this;
        }

        public Cursor EmitCallvirt(MethodReference target) {
            Emit(il.Create(OpCodes.Callvirt, target));
            return this;
        }
        // public Cursor EmitBrFalse(RMLabel target) {
            // Emit(IL.Create(OpCodes.Brfalse, target.Target));
            // return this;
        // }

        public Cursor EmitBrFalse(Instruction target) {
            Emit(il.Create(OpCodes.Brfalse, target));
            return this;
        }

        public Cursor EmitBrTrue(Instruction target) {
            Emit(il.Create(OpCodes.Brtrue, target));
            return this;
        }
        public Cursor EmitBr(Instruction target) {
            Emit(il.Create(OpCodes.Br, target));
            return this;
        }
        public Cursor EmitNewobj(MethodReference ctor) {
            Emit(il.Create(OpCodes.Newobj, ctor));
            return this;
        }
        // public Cursor EmitNewobj(MethodBase ctor);
        public Cursor EmitLdtoken(TypeReference type) {
            Emit(il.Create(OpCodes.Ldtoken, type));
            return this;
        }

        public Cursor EmitLdstr(string s) {
            Emit(il.Create(OpCodes.Ldstr, s));
            return this;
        }

        public Cursor EmitPop() {
            Emit(il.Create(OpCodes.Pop));
            return this;
        }

        public Cursor EmitRet() {
            Emit(il.Create(OpCodes.Ret));
            return this;
        }

        [Obsolete("Use EmitCall or EmitCallvirt")]
        public Cursor EmitDelegate<T>(T cb) where T : Delegate {
            throw new NotSupportedException();
        }

        public VariableDefinition CreateLocal(TypeReference type) {
            VariableDefinition varDef = new(type);
            body.AddVariable(varDef);
            return varDef;
        }

        public VariableDefinition GetLocal(int index) {
            ArgumentOutOfRangeException.ThrowIfNegative(index);
            ArgumentOutOfRangeException.ThrowIfGreaterThanOrEqual(index, body.Variables.Count);
            return body.Variables[index];
        }

        public Cursor ClearLocals() {
            body.Variables.Clear();
            return this;
        }
        
        
    }
    
    public static void LogAllInstrs(LegCursor il) => LogAllInstrs(il.Context);
    public static void LogAllInstrs(MethodPatchContext il) {
        Func<StringBuilder, Instruction, StringBuilder> logInstr =
            typeof(ILContext).GetMethod("ToString", BindingFlags.Static | BindingFlags.NonPublic)!
                .CreateDelegate<Func<StringBuilder, Instruction, StringBuilder>>();
        Console.WriteLine("Logging instructions");
        Console.WriteLine("In method " + il.Method.FullName);
        StringBuilder s = new();
        for (int i = 0; i < il.Method.Body.Instructions.Count; i++) {
            Instruction? instr = il.Method.Body.Instructions[i];
            try {
                s.Append($"{i} ");
                logInstr.Invoke(s, instr);
                if (instr.Operand is Instruction jInstr) {
                    s.Remove(s.Length - 1, 1);
                    s.AppendLine($" -> {il.Method.Body.Instructions.IndexOf(jInstr)}");
                }
            }
            catch (InvalidCastException) {
                Console.WriteLine("Unknown instr");
            }
        }

        Console.WriteLine(s.ToString());
        Console.WriteLine("Logging instructions end");
    }
}
