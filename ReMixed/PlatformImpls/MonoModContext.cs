using System;
using System.Linq;
using System.Reflection;
using System.Text;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Collections.Generic;
using MonoMod.Cil;
using MethodBody = Mono.Cecil.Cil.MethodBody;

namespace ReMixed.PlatformImpls;

public class MonoModContext : IPatchContext {
    private readonly ILContext mmContext;
    private readonly MethodBase? origMethod;
    public IPatchContext.Cursor ContextCursor { get; }

    private StackAnalysis? analysis;
    public StackAnalysis StAnalysis => analysis ??= new StackAnalysis(mmContext.Method, StAnalysisConvert);
    private MethodDefinition? origMethodDef;

    private InjectionTracker? injectionTracker;

    // Lazy loaded to remove stateful calls from the ctor
    public InjectionTracker InjectionTracker =>
        injectionTracker ??= new InjectionTracker(GetRealMethod().Body, StAnalysis);

    private MonoModContext(ILContext ctx) {
        mmContext = ctx;
        ContextCursor = new MonoModCursor(new ILCursor(ctx), this);
    }

    public MonoModContext(ILContext ctx, MethodBase origMethodBase) : this(ctx) {
        origMethod = origMethodBase;
    }

    public MonoModContext(ILContext ctx, MethodDefinition origMethodDefinition) : this(ctx) {
        origMethodDef = origMethodDefinition;
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
        if (origMethod != null)
            origMethodDef ??= mmContext.Module.ImportReference(origMethod).Resolve();
        if (origMethodDef == null) throw new InvalidOperationException();
        if (mmContext.Method == origMethodDef) {
            
        }
        return origMethodDef;
    }

    public MethodReference ImportMethod(MethodBase method) {
        return mmContext.Import(method);
    }

    public class MonoModCursor : IPatchContext.Cursor {
        private readonly ILCursor mmCursor;

        // TODO: Make this only accessible from the surrounding classes
        public MonoModCursor(ILCursor cursor, IPatchContext context) {
            mmCursor = cursor;
            Context = context;
        }

        public MonoModCursor(MonoModContext patchContext) {
            mmCursor = new ILCursor(patchContext.mmContext);
            Context = patchContext;
        }

        public object _RealType => mmCursor;
        public Instruction? Next => mmCursor.Next;
        public Instruction? Previous => mmCursor.Previous;

        public MethodDefinition Method => mmCursor.Method;
        
        public IPatchContext Context { get; }

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

        public IPatchContext.Cursor GotoNext(params Predicate<Instruction>[] predicates) {
            if (predicates.Length == 0) {
                this.MoveIndex(1); // For some reason, empty predicates do not move the cursor ._.
            }
            Func<Instruction, bool>[] monoModPredicates = FixPredicateArray(predicates);
            mmCursor.GotoNext(monoModPredicates);
            return this;
        }

        public IPatchContext.Cursor GotoFirst() {
            mmCursor.Goto(0);
            return this;
        }

        public IPatchContext.Cursor GotoLast() {
            mmCursor.Goto(-1);
            return this;
        }

        public IPatchContext.Cursor GotoInstr(Instruction? target) {
            mmCursor.Goto(target);
            return this;
        }

        public IPatchContext.Cursor MoveIndex(int move) {
            mmCursor.Goto(mmCursor.Index + move);
            return this;
        }

        public IPatchContext.Cursor RemoveNext() {
            mmCursor.Remove();
            return this;
        }

        public IPatchContext.Cursor Emit(OpCode opcode, object operand) {
            mmCursor.Emit(opcode, operand);
            return this;
        }

        public IPatchContext.Cursor EmitDup() {
            mmCursor.EmitDup();
            return this;
        }

        public IPatchContext.Cursor EmitLdcI4(int value) {
            mmCursor.EmitLdcI4(value);
            return this;
        }

        public IPatchContext.Cursor EmitLdcI8(long value) {
            mmCursor.EmitLdcI8(value);
            return this;
        }

        public IPatchContext.Cursor EmitLdcR4(float value) {
            mmCursor.EmitLdcR4(value);
            return this;
        }

        public IPatchContext.Cursor EmitLdcR8(double value) {
            mmCursor.EmitLdcR8(value);
            return this;
        }

        public IPatchContext.Cursor EmitLdLoc(VariableReference reference) {
            mmCursor.EmitLdloc(reference);
            return this;
        }
        
        public IPatchContext.Cursor EmitStLoc(VariableReference reference) {
            mmCursor.EmitStloc(reference);
            return this;
        }

        public IPatchContext.Cursor EmitLdarg0() {
            mmCursor.EmitLdarg0();
            return this;
        }
        
        public IPatchContext.Cursor EmitLdarg(int index) {
            int shift = 0;
            // MonoMod will add an explicit this to the DMD method and make it static, as such,
            // if the orig method was instance, skip the first argument
            if (Context.GetRealMethod().HasThis) {
                shift = 1;
            }
            mmCursor.EmitLdarg(Method.Parameters[index + shift]);
            return this;
        }

        public IPatchContext.Cursor EmitCall(MethodReference target) {
            mmCursor.EmitCall(target);
            return this;
        }

        public IPatchContext.Cursor EmitCallvirt(MethodReference target) {
            mmCursor.EmitCallvirt(target);
            return this;
        }

        public IPatchContext.Cursor EmitBrFalse(IPatchContext.ILLabel target) {
            mmCursor.EmitBrfalse((ILLabel)target._RealType);
            return this;
        }

        public IPatchContext.Cursor EmitBrTrue(IPatchContext.ILLabel target) {
            mmCursor.EmitBrtrue((ILLabel)target._RealType);
            return this;
        }

        public IPatchContext.Cursor EmitBr(IPatchContext.ILLabel target) {
            mmCursor.EmitBr((ILLabel)target._RealType);
            return this;
        }
        
        public IPatchContext.Cursor EmitNewobj(MethodReference ctor) {
            mmCursor.EmitNewobj(ctor);
            return this;
        }
        
        public IPatchContext.Cursor EmitNewobj(MethodBase ctor) {
            mmCursor.EmitNewobj(ctor);
            return this;
        }

        public IPatchContext.Cursor EmitLdtoken(Type type) {
            mmCursor.EmitLdtoken(type);
            return this;
        }

        public IPatchContext.Cursor EmitPop() {
            mmCursor.EmitPop();
            return this;
        }

        public IPatchContext.Cursor EmitRet() {
            mmCursor.EmitRet();
            return this;
        }

        public IPatchContext.Cursor EmitDelegate<T>(T cb) where T : Delegate {
            mmCursor.EmitDelegate(cb);
            return this;
        }

        public IPatchContext.ILLabel GetLabel() {
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
    
    public class MonoModILLabel(ILLabel label) : IPatchContext.ILLabel {
        public object _RealType => label;
        public Instruction? Target => label.Target;
    }

    public static void LogAllInstrs(IPatchContext.Cursor il) => LogAllInstrs((ILCursor)il._RealType);
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
}
