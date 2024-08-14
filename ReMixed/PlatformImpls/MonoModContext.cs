using System;
using System.Linq;
using System.Reflection;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Collections.Generic;
using MonoMod.Cil;
using MethodBody = Mono.Cecil.Cil.MethodBody;

namespace ReMixed.PlatformImpls;

public class MonoModContext : MethodPatchContext {
    private readonly ILContext mmContext;
    private readonly MethodBase? origMethod;

    private MethodDefinition? origMethodDef;

    private MonoModContext(ILContext ctx) : base(ctx.Method) {
        mmContext = ctx;
    }

    public MonoModContext(ILContext ctx, MethodBase origMethodBase) : this(ctx) {
        origMethod = origMethodBase;
    }

    protected override Func<object, MethodBody, Collection<Instruction>, object> GetStAnalysisConverter()
        => StAnalysisConvert;

    public static object StAnalysisConvert(object operand, MethodBody bo, Collection<Instruction> instrs) {
        return operand switch {
            // For some reason monomod may add ILLabels instead of Instruction to MethodBodies
            ILLabel ilLabel => instrs[bo.Instructions.IndexOf(ilLabel.Target)],
            ILLabel[] ilLabels => ilLabels.Select(label => instrs[bo.Instructions.IndexOf(label.Target)]).ToArray(),
            _ => operand
        };
    }
    
    // MonoMod returns a `DMD` method in context.Method, consequently certain properties of it (such as `IsStatic`) are lost
    public override MethodDefinition GetRealMethod() {
        if (origMethod != null)
            origMethodDef ??= mmContext.Module.ImportReference(origMethod).Resolve();
        if (origMethodDef == null) throw new InvalidOperationException();
        if (mmContext.Method == origMethodDef) {
            
        }
        return origMethodDef;
    }

    public override MethodReference ImportMethod(MethodBase method) {
        return mmContext.Import(method);
    }

    // public class MonoModCursor : Cursor {
        // private readonly ILCursor mmCursor;
        //
        // public MonoModCursor(MonoModContext patchContext) : base(patchContext) {
        //     mmCursor = new ILCursor(patchContext.mmContext);
        // }
        //
        // public override object _RealType => mmCursor;
        // public Instruction? Next => mmCursor.Next;
        // public Instruction? Previous => mmCursor.Previous;
        //
        // public MethodDefinition Method => mmCursor.Method;
        //
        // public PatchContext Context { get; }
        //
        // private Func<Instruction, bool>[] FixPredicateArray(Predicate<Instruction>[] predicates) {
        //     Func<Instruction, bool>[] monoModPredicates = new Func<Instruction, bool>[predicates.Length];
        //     for (int i = 0; i < predicates.Length; i++) {
        //         Predicate<Instruction> pred = predicates[i];
        //         monoModPredicates[i] = instruction => pred(instruction);
        //     }
        //
        //     return monoModPredicates;
        // }
        //
        // public bool TryGotoNext(params Predicate<Instruction>[] predicates) {
        //     Func<Instruction, bool>[] monoModPredicates = FixPredicateArray(predicates);
        //     return mmCursor.TryGotoNext(monoModPredicates);
        // }
        //
        // public Cursor GotoNext(params Predicate<Instruction>[] predicates) {
        //     if (predicates.Length == 0) {
        //         MoveIndex(1); // For some reason, empty predicates do not move the cursor ._.
        //     }
        //     Func<Instruction, bool>[] monoModPredicates = FixPredicateArray(predicates);
        //     mmCursor.GotoNext(monoModPredicates);
        //     return this;
        // }
        //
        // public Cursor GotoFirst() {
        //     mmCursor.Goto(0);
        //     return this;
        // }
        //
        // public Cursor GotoLast() {
        //     mmCursor.Goto(-1);
        //     return this;
        // }
        //
        // public Cursor GotoInstr(Instruction? target) {
        //     mmCursor.Goto(target);
        //     return this;
        // }
        //
        // public Cursor MoveIndex(int move) {
        //     mmCursor.Goto(mmCursor.Index + move);
        //     return this;
        // }
        //
        // public Cursor RemoveNext() {
        //     mmCursor.Remove();
        //     return this;
        // }
        //
        // public Cursor Emit(OpCode opcode, object operand) {
        //     mmCursor.Emit(opcode, operand);
        //     return this;
        // }
        //
        // public Cursor EmitDup() {
        //     mmCursor.EmitDup();
        //     return this;
        // }
        //
        // public Cursor EmitLdcI4(int value) {
        //     mmCursor.EmitLdcI4(value);
        //     return this;
        // }
        //
        // public Cursor EmitLdcI8(long value) {
        //     mmCursor.EmitLdcI8(value);
        //     return this;
        // }
        //
        // public Cursor EmitLdcR4(float value) {
        //     mmCursor.EmitLdcR4(value);
        //     return this;
        // }
        //
        // public Cursor EmitLdcR8(double value) {
        //     mmCursor.EmitLdcR8(value);
        //     return this;
        // }
        //
        // public Cursor EmitLdLoc(VariableReference reference) {
        //     mmCursor.EmitLdloc(reference);
        //     return this;
        // }
        //
        // public Cursor EmitStLoc(VariableReference reference) {
        //     mmCursor.EmitStloc(reference);
        //     return this;
        // }
        //
        // public Cursor EmitLdarg0() {
        //     mmCursor.EmitLdarg0();
        //     return this;
        // }
        //
        // public Cursor EmitLdarg(int index) {
        //     int shift = 0;
        //     // MonoMod will add an explicit this to the DMD method and make it static, as such,
        //     // if the orig method was instance, skip the first argument
        //     if (Context.GetRealMethod().HasThis) {
        //         shift = 1;
        //     }
        //     mmCursor.EmitLdarg(Method.Parameters[index + shift]);
        //     return this;
        // }
        //
        // public Cursor EmitCall(MethodReference target) {
        //     mmCursor.EmitCall(target);
        //     return this;
        // }
        //
        // public Cursor EmitCallvirt(MethodReference target) {
        //     mmCursor.EmitCallvirt(target);
        //     return this;
        // }
        //
        // public Cursor EmitBrFalse(RMLabel target) {
        //     mmCursor.EmitBrfalse((ILLabel)target._RealType);
        //     return this;
        // }
        //
        // public Cursor EmitBrTrue(RMLabel target) {
        //     mmCursor.EmitBrtrue((ILLabel)target._RealType);
        //     return this;
        // }
        //
        // public Cursor EmitBr(RMLabel target) {
        //     mmCursor.EmitBr((ILLabel)target._RealType);
        //     return this;
        // }
        //
        // public Cursor EmitNewobj(MethodReference ctor) {
        //     mmCursor.EmitNewobj(ctor);
        //     return this;
        // }
        //
        // public Cursor EmitNewobj(MethodBase ctor) {
        //     mmCursor.EmitNewobj(ctor);
        //     return this;
        // }
        //
        // public Cursor EmitLdtoken(Type type) {
        //     mmCursor.EmitLdtoken(type);
        //     return this;
        // }
        //
        // public Cursor EmitPop() {
        //     mmCursor.EmitPop();
        //     return this;
        // }
        //
        // public Cursor EmitRet() {
        //     mmCursor.EmitRet();
        //     return this;
        // }
        //
        // public Cursor EmitDelegate<T>(T cb) where T : Delegate {
        //     mmCursor.EmitDelegate(cb);
        //     return this;
        // }
        //
        // public RMLabel GetLabel() {
        //     MonoModILLabel mmILL =  new(mmCursor.MarkLabel());
        //     mmCursor.MoveBeforeLabels();
        //     return mmILL;
        // }
        //
        // public VariableDefinition CreateLocal(Type type) {
        //     VariableDefinition vd = new(mmCursor.Context.Import(type));
        //     mmCursor.Context.Body.Variables.Add(vd);
        //     return vd;
        // }
    // }
    
    
}
