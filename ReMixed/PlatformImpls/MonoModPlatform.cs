using System;
using System.Linq;
using System.Reflection;
using System.Text;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Collections.Generic;
using MonoMod.Cil;
using MonoMod.RuntimeDetour;
using MethodBody = Mono.Cecil.Cil.MethodBody;

namespace ReMixed.PlatformImpls;

public class MonoModPlatform : PatchPlatform {
    public string Name => "MonoMod";
    private readonly ILContext mmContext;
    private readonly MethodBase origMethod;
    public PatchPlatform.Cursor PlatformCursor { get; }

    private StackAnalysis? analysis;
    public StackAnalysis StAnalysis => analysis ??= new StackAnalysis(mmContext.Method, StAnalysisConvert);
    private MethodDefinition? origMethodDef;

    public InjectionTracker InjectionTracker { get; }

    public MonoModPlatform(ILContext ctx, MethodBase origMethod) {
        mmContext = ctx;
        this.origMethod = origMethod;
        PlatformCursor = new MonoModCursor(new ILCursor(ctx), this);
        InjectionTracker = new InjectionTracker(GetRealMethod().Body, StAnalysis);
    }

    private static object StAnalysisConvert(object operand, MethodBody bo, Collection<Instruction> instrs) {
        return operand switch {
            // For some reason monomod may add ILLabels instead of Instruction to MethodBodies
            ILLabel ilLabel => instrs[bo.Instructions.IndexOf(ilLabel.Target)],
            ILLabel[] ilLabels => ilLabels.Select(label => instrs[bo.Instructions.IndexOf(label.Target)]).ToArray(),
            _ => operand
        };
    }
    
    // MonoMod returns a `DMD` method in context.Method, consequently certain properties of it (such as `IsStatic`) are lost
    public MethodDefinition GetRealMethod() {
        origMethodDef ??= mmContext.Module.ImportReference(origMethod).Resolve();
        return origMethodDef;
    }

    public int GetInstructionIdxInOrig(Instruction instruction) {
        return 0;
    }

    public class MonoModCursor : PatchPlatform.Cursor {
        private readonly ILCursor mmCursor;

        // TODO: Make this only accessible from the surrounding classes
        public MonoModCursor(ILCursor cursor, PatchPlatform platform) {
            mmCursor = cursor;
            Platform = platform;
        }

        public MonoModCursor(MonoModPlatform patchPlatform) {
            mmCursor = new ILCursor(patchPlatform.mmContext);
            Platform = patchPlatform;
        }

        public object _RealType => mmCursor;
        public Instruction? Next => mmCursor.Next;
        public Instruction? Previous => mmCursor.Previous;

        public MethodDefinition Method => mmCursor.Method;
        
        public PatchPlatform Platform { get; }

        private Func<Instruction, bool>[] FixPredicateArray(Predicate<Instruction>[] predicates) {
            Func<Instruction, bool>[] monoModPredicates = new Func<Instruction, bool>[predicates.Length];
            for (int i = 0; i < predicates.Length; i++) {
                Predicate<Instruction> pred = predicates[i];
                monoModPredicates[i] = instruction => pred(instruction);
            }

            return monoModPredicates;
        }

        public bool TryGotoNext(params Predicate<Instruction>[] predicates) {
            Func<Instruction, bool>[] monoModPredicates = FixPredicateArray(predicates);
            return mmCursor.TryGotoNext(monoModPredicates);
        }

        public PatchPlatform.Cursor GotoNext(params Predicate<Instruction>[] predicates) {
            if (predicates.Length == 0) {
                this.MoveIndex(1); // For some reason, empty predicates do not move the cursor ._.
            }
            Func<Instruction, bool>[] monoModPredicates = FixPredicateArray(predicates);
            mmCursor.GotoNext(monoModPredicates);
            return this;
        }

        public PatchPlatform.Cursor GotoFirst() {
            mmCursor.Goto(0);
            return this;
        }

        public PatchPlatform.Cursor GotoLast() {
            mmCursor.Goto(-1);
            return this;
        }

        public PatchPlatform.Cursor GotoInstr(Instruction? target) {
            mmCursor.Goto(target);
            return this;
        }

        public PatchPlatform.Cursor MoveIndex(int move) {
            mmCursor.Goto(mmCursor.Index + move);
            return this;
        }

        public PatchPlatform.Cursor RemoveNext() {
            mmCursor.Remove();
            return this;
        }

        public PatchPlatform.Cursor Emit(OpCode opcode, object operand) {
            mmCursor.Emit(opcode, operand);
            return this;
        }

        public PatchPlatform.Cursor EmitDup() {
            mmCursor.EmitDup();
            return this;
        }

        public PatchPlatform.Cursor EmitLdcI4(int value) {
            mmCursor.EmitLdcI4(value);
            return this;
        }

        public PatchPlatform.Cursor EmitLdcI8(long value) {
            mmCursor.EmitLdcI8(value);
            return this;
        }

        public PatchPlatform.Cursor EmitLdcR4(float value) {
            mmCursor.EmitLdcR4(value);
            return this;
        }

        public PatchPlatform.Cursor EmitLdcR8(double value) {
            mmCursor.EmitLdcR8(value);
            return this;
        }

        public PatchPlatform.Cursor EmitLdLoc(VariableReference reference) {
            mmCursor.EmitLdloc(reference);
            return this;
        }
        
        public PatchPlatform.Cursor EmitStLoc(VariableReference reference) {
            mmCursor.EmitStloc(reference);
            return this;
        }

        public PatchPlatform.Cursor EmitLdarg0() {
            mmCursor.EmitLdarg0();
            return this;
        }
        
        public PatchPlatform.Cursor EmitLdarg(int index) {
            int shift = 0;
            // MonoMod will add an explicit this to the DMD method and make it static, as such,
            // if the orig method was instance, skip the first argument
            if (Platform.GetRealMethod().HasThis) {
                shift = 1;
            }
            mmCursor.EmitLdarg(Method.Parameters[index + shift]);
            return this;
        }

        public PatchPlatform.Cursor EmitCall(MethodBase target) {
            mmCursor.EmitCall(target);
            return this;
        }

        public PatchPlatform.Cursor EmitCallvirt(MethodBase target) {
            mmCursor.EmitCallvirt(target);
            return this;
        }

        public PatchPlatform.Cursor EmitBrFalse(PatchPlatform.ILLabel target) {
            mmCursor.EmitBrfalse((ILLabel)target._RealType);
            return this;
        }

        public PatchPlatform.Cursor EmitBrTrue(PatchPlatform.ILLabel target) {
            mmCursor.EmitBrtrue((ILLabel)target._RealType);
            return this;
        }

        public PatchPlatform.Cursor EmitBr(PatchPlatform.ILLabel target) {
            mmCursor.EmitBr((ILLabel)target._RealType);
            return this;
        }
        
        public PatchPlatform.Cursor EmitNewobj(MethodBase ctor) {
            mmCursor.EmitNewobj(ctor);
            return this;
        }

        public PatchPlatform.Cursor EmitLdtoken(Type type) {
            mmCursor.EmitLdtoken(type);
            return this;
        }

        public PatchPlatform.Cursor EmitPop() {
            mmCursor.EmitPop();
            return this;
        }

        public PatchPlatform.Cursor EmitRet() {
            mmCursor.EmitRet();
            return this;
        }

        public PatchPlatform.Cursor EmitDelegate<T>(T cb) where T : Delegate {
            mmCursor.EmitDelegate(cb);
            return this;
        }

        public PatchPlatform.ILLabel GetLabel() {
            MonoModILLabel mmILL =  new(mmCursor.MarkLabel());
            mmCursor.MoveBeforeLabels();
            return mmILL;
        }

        public VariableDefinition CreateLocal(Type type) {
            VariableDefinition vd = new(mmCursor.Context.Import(type));
            mmCursor.Context.Body.Variables.Add(vd);
            return vd;
        }
    }
    
    public class MonoModILLabel(ILLabel label) : PatchPlatform.ILLabel {
        public object _RealType => label;
        public Instruction? Target => label.Target;
    }

    public static void LogAllInstrs(PatchPlatform.Cursor il) => LogAllInstrs((ILCursor)il._RealType);
    public static void LogAllInstrs(ILCursor il) {
        Func<StringBuilder, Instruction, StringBuilder> logInstr =
            typeof(ILContext).GetMethod("ToString", BindingFlags.Static | BindingFlags.NonPublic)!
                .CreateDelegate<Func<StringBuilder, Instruction, StringBuilder>>();
        Console.WriteLine("Logging instructions");
        Console.WriteLine("In method " + il.Method.FullName);
        StringBuilder s = new();
        foreach (Instruction? instr in il.Instrs) {
            try {
                logInstr.Invoke(s, instr);
            }
            catch (InvalidCastException) {
                Console.WriteLine("Unknown instr");
            }
        }

        Console.WriteLine(s.ToString());
        Console.WriteLine("Logging instructions end");
    }
    
    public static ILHook AutoHook(MethodBase method, Action<PatchPlatform.Cursor> action) {
        return new ILHook(method, GetManipulator(action, method));
    }

    protected static ILContext.Manipulator GetManipulator(Action<PatchPlatform.Cursor> action, MethodBase method) => ctx => {
        MonoModPlatform mmPlat = new(ctx, method);
        MonoModCursor cursor = new(mmPlat);
        action(cursor);
    };
}
