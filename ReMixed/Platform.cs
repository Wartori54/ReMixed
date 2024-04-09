using System;
using System.Reflection;
using Mono.Cecil;
using Mono.Cecil.Cil;

namespace ReMixed;

public interface PatchPlatform {

    public string Name { get; }
    public Cursor PlatformCursor { get; }
    
    public StackAnalysis StAnalysis { get; }
    
    public InjectionTracker InjectionTracker { get; }

    public int GetInstructionIdxInOrig(Instruction instruction);

    /// <summary>
    /// Obtains the target method's MethodDefinition, this is necessary since in some platform the modified and the
    /// target method may not be the same.
    /// </summary>
    /// <returns></returns>
    public MethodDefinition GetRealMethod();
    
    public interface Cursor {
        public object _RealType { get; }
        
        public Instruction? Next { get; }
        public Instruction? Previous { get; }
        
        public MethodDefinition Method { get; }
        public PatchPlatform Platform { get; }

        public bool TryGotoNext(params Predicate<Instruction>[] predicates);
        public Cursor GotoNext(params Predicate<Instruction>[] predicates);

        public Cursor GotoFirst();
        public Cursor GotoLast();

        public Cursor MoveIndex(int move);
        
        public Cursor GotoInstr(Instruction? target);

        public Cursor RemoveNext();

        public Cursor Emit(OpCode opcode, object operand);
        public Cursor EmitDup();
        public Cursor EmitLdcI4(int value);
        public Cursor EmitLdcI8(long value);
        public Cursor EmitLdcR4(float value);
        public Cursor EmitLdcR8(double value);
        public Cursor EmitLdLoc(VariableReference reference);
        public Cursor EmitStLoc(VariableReference reference);
        public Cursor EmitLdarg0();
        /// <summary>
        /// Emits the load instruction for the i-th argument.
        /// WARNING: This skips over the instance in case its available.
        /// </summary>
        public Cursor EmitLdarg(int index);
        public Cursor EmitCall(MethodBase target);
        public Cursor EmitCallvirt(MethodBase target);
        public Cursor EmitBrFalse(ILLabel target);
        public Cursor EmitBrTrue(ILLabel target);
        public Cursor EmitBr(ILLabel target);
        public Cursor EmitNewobj(MethodBase ctor);
        public Cursor EmitLdtoken(Type type);

        public Cursor EmitPop();

        public Cursor EmitRet();

        [Obsolete("Prefer EmitCall or EmitCallvirt")]
        public Cursor EmitDelegate<T>(T cb) where T : Delegate;

        public ILLabel GetLabel();

        public VariableDefinition CreateLocal(Type type);
    }
    
    public interface ILLabel {
        public object _RealType { get; }
        public Instruction? Target { get; }
    }
}
