using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Runtime.InteropServices;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Collections.Generic;
using MonoMod.Cil;
using MonoMod.Utils;

namespace ReMixed;

public class StackAnalysis {
    /// <summary>
    /// This converts an operand (arg 1) from an instruction in the first body (arg 2) to a standardized type
    /// with the metadata associated to the second body (instr list) (arg 3).
    /// Blame MonoMod for this
    /// </summary>
    public readonly Func<object, MethodBody, Collection<Instruction>, object>? OperandConverter;
    public MethodDefinition TargetMethod { get; }

    private readonly Collection<Instruction> methodInstructions;

    private Collection<StackFrame>? stackFrames;
    // The i-th stackframe corresponds to the stack state before the i-th instruction
    public Collection<StackFrame> StackFrames {
        get {
            if (stackFrames == null)
                Perform();
            return stackFrames;
        }
    }

    private readonly Dictionary<int, List<int>> branches = [];
    // Dict of end to start indexes of branches
    public Dictionary<int, List<int>> Branches {
        get {
            if (stackFrames == null)
                Perform();
            return branches;
        }
    }

    public StackAnalysis(MethodDefinition method, Func<object, MethodBody, Collection<Instruction>, object>? operandConverter = null) {
        OperandConverter = operandConverter;
        TargetMethod = method;
        methodInstructions = CopyInstructions(method.Body, operandConverter);
    }

    /// <summary>
    /// Preforms a scan on the current `MethodBody`, creating a `StackFrame` for each possible state before every instruction
    /// </summary>
    /// <exception cref="ArgumentOutOfRangeException"></exception>
    [MemberNotNull(nameof(stackFrames))]
    private void Perform() {
        stackFrames = new Collection<StackFrame>(methodInstructions.Count + 1);
        for (int i = 0; i < methodInstructions.Count + 1; i++) {
            stackFrames.Add(StackFrame.Null());
        }
        Console.WriteLine($"Performing stack analysis for method {TargetMethod.FullName}");
        stackFrames[0] = StackFrame.Empty(); // The stack always starts empty
        StackFrame curr = stackFrames[0];
        Dictionary<int, StackFrame> jumps = new();
        for (int i = 0; i < methodInstructions.Count; i++) {
            Instruction instr = methodInstructions[i];
            OpCode op = instr.OpCode;
            int stackRes;
            {
                int stPop = GetStackRes(op.StackBehaviourPop);
                int stPush = GetStackRes(op.StackBehaviourPush);
                if (stPop == int.MinValue) stackRes = stPop;
                else if (stPush == int.MaxValue) stackRes = stPush;
                else stackRes = stPush + stPop;
            }

            switch (op.FlowControl) {
                case FlowControl.Branch:
                    if (stackRes == int.MinValue || stackRes == int.MaxValue) {
                        throw new InvalidOperationException();
                    }

                    if (op == OpCodes.Br || op == OpCodes.Br_S) {
                        AddJump(i, (Instruction)methodInstructions[i].Operand, curr, stackRes, jumps);
                        // Fast forward to the next jump-able instruction, since code after br is not reachable if nothing jumps to it
                        i++;
                        while (!jumps.TryGetValue(i, out _)) {
                            i++;
                        }

                        i--; // Move to the last dead instr, since the loop will advance one
                    } else {
                        throw new NotImplementedException();
                    }
                    
                    break;
                case FlowControl.Break: // No-ops
                    stackFrames[i + 1] = curr;
                    break;
                case FlowControl.Call:
                    stackFrames[i + 1] = curr = ParseCall(curr, instr, op, stackRes);
                    break;
                case FlowControl.Cond_Branch:
                    if (stackRes == int.MinValue || stackRes == int.MaxValue) {
                        throw new InvalidOperationException();
                    }
                    // operand is InlineBrTarget or ShortInlineBrTarget or InlineSwitch
                    if (op.OperandType == OperandType.InlineSwitch) { // operand is Instruction[]
                        Instruction[] switchJumps = (Instruction[])methodInstructions[i].Operand;
                        foreach (Instruction switchJump in switchJumps) {
                            AddJump(i, switchJump, curr, stackRes, jumps);
                        }
                    } else {
                        AddJump(i, (Instruction)methodInstructions[i].Operand, curr, stackRes, jumps);
                    }
                    stackFrames[i + 1] = curr = curr.PushType(stackRes);
                    break;
                case FlowControl.Meta:
                    if (op.OpCodeType == OpCodeType.Prefix) {
                        throw new NotImplementedException();
                    } else {
                        throw new InvalidOperationException(
                            $"{nameof(FlowControl.Meta)} can only be applied to {OpCodeType.Prefix}");
                    }
                    break;
                case FlowControl.Next: // Most instructions fall here
                    if (stackRes == int.MinValue || stackRes == int.MaxValue)
                        throw new InvalidOperationException(
                            $"Non-fixed {nameof(StackBehaviour)} can only be applied to {nameof(FlowControl.Call)} opcodes!");
                    stackFrames[i + 1] = curr = curr.PushType(stackRes);
                    break;
                case FlowControl.Phi: // Unused
                    stackFrames[i + 1] = curr;
                    break;
                case FlowControl.Return:
                    if (stackRes == int.MaxValue) {
                        throw new InvalidOperationException(
                            $"No operation may have {nameof(FlowControl.Return)} and {nameof(StackBehaviour.Varpush)}!");
                    }

                    if (stackRes == int.MinValue) { // It has to be ret, so it pops a single value if not void
                        if (op != OpCodes.Ret) throw new InvalidOperationException();
                        stackFrames[i + 1] = curr = 
                            curr.PopType(TargetMethod.ReturnType.FullName == typeof(void).FullName ? 0 : 1);
                    } else {
                        stackFrames[i + 1] = curr = curr.PushType(stackRes);
                    }
                    break;
                case FlowControl.Throw:
                    if (stackRes == int.MinValue || stackRes == int.MaxValue)
                        throw new InvalidOperationException(
                            $"No operation may have {nameof(FlowControl.Throw)} with non-fixed {nameof(StackBehaviour)}");
                    stackFrames[i + 1] = curr = curr.PushType(stackRes);
                    break;
                default:
                    throw new ArgumentOutOfRangeException();
            }
            
            // jump stack merge handling
            if (!jumps.TryGetValue(i+1, out StackFrame? jmpFrame)) continue;

            if (!curr.Equals(jmpFrame)) { // The stack types MUST match, if it doesnt its invalid IL
                throw new InvalidOperationException("Invalid IL detected!");
            }

        }

        Console.WriteLine($"Final stack frame: {stackFrames[methodInstructions.Count].stackAmount}");
    }

    private static StackFrame ParseCall(StackFrame curr, Instruction instr, OpCode op, int stackRes) {
        if (stackRes != int.MinValue && stackRes != int.MaxValue) {
            // Its stack behaviour is always consistent
            return curr.PushType(stackRes);
        }

        if (op.StackBehaviourPop == StackBehaviour.PopAll) { // This nukes the stack
            return StackFrame.Empty();
        }

        int realSb = 0;
        if (op.OperandType == OperandType.InlineMethod || // InlineMethod implies a MethodReference
            op.OperandType == OperandType.InlineSig) { // InlineSig implies a CallSite
            IMethodSignature mr = (IMethodSignature)instr.Operand; // Both are IMethodSignature
            if (op.StackBehaviourPop == StackBehaviour.Varpop) {
                // Newobj has a this and its not explicit, but it does not consume anything anyway TODO: remove edge case
                if (mr.HasThis && !mr.ExplicitThis && op.Code != Code.Newobj) realSb--;
                realSb -= mr.Parameters.Count; // Pops all args
            } else {
                realSb += GetStackRes(op.StackBehaviourPop);
            }

            if (op.StackBehaviourPush == StackBehaviour.Varpush) {
                if (mr.ReturnType.FullName !=
                    typeof(void).FullName) // Void does not push to the stack
                    realSb++;
            } else {
                realSb += GetStackRes(op.StackBehaviourPush); 
            }
        } else {
            throw new InvalidOperationException("Unknown Call-operandType combo!");
        }

        return curr.PushType(realSb);
    }

    private void AddJump(int i, Instruction jmp, StackFrame curr, int stackRes, Dictionary<int, StackFrame> jumps) {
        int jmpIdx = methodInstructions.IndexOf(jmp);
        if (jmpIdx == -1) {
            throw new InvalidOperationException("Invalid jump target?");
        }
        if (!Branches.TryGetValue(jmpIdx, out List<int>? possibleJumps))
            Branches.Add(jmpIdx, [i]);
        else
            possibleJumps.Add(i);

        StackFrame nStFrame = curr.PushType(stackRes);
        if (jumps.TryAdd(jmpIdx, nStFrame)) return;
        // Conflict: two branches jump to the same point
        if (!jumps[jmpIdx].Equals(nStFrame)) { // They MUST have the same stack types
            throw new InvalidOperationException("Invalid IL detected!");
            // jumps[jmpIdx].MergeWith(nStFrame);
        } 
    }

    public static Collection<Instruction> CopyInstructions(MethodBody bo, Func<object, MethodBody, Collection<Instruction>, object>? operandConverter) {
        Collection<Instruction> copy = [];
        copy.AddRange(bo.Instructions.Select(o => {
            Instruction c = Instruction.Create(OpCodes.Nop);
            c.OpCode = o.OpCode;
            c.Operand = o.Operand;
            c.Offset = o.Offset;
            return c;
        }));

        foreach (Instruction c in copy) {
            if (c.Operand is Instruction target) {
                c.Operand = copy[bo.Instructions.IndexOf(target)];
            } else if (c.Operand is Instruction[] targets) {
                c.Operand = targets.Select(i => copy[bo.Instructions.IndexOf(i)]).ToArray();
            } else if (operandConverter != null) {
                c.Operand = operandConverter(c.Operand, bo, copy);
            }
            
        }

        return copy;
    }

    /// <summary>
    /// Gets the amount of entries pushed or poped to/from the stack
    /// </summary>
    /// <param name="sb">The stack behaviour</param>
    /// <returns>An integer value. It may be `int.MinValue` in case it pops all values or it is dependent on the operand,
    /// or `int.MaxValue` in case the amount of elements pushed depends on the operand.</returns>
    /// <exception cref="ArgumentOutOfRangeException"></exception>
    public static int GetStackRes(StackBehaviour sb) {
        return sb switch {
            StackBehaviour.Pop0 => 0,
            StackBehaviour.Pop1 => -1,
            StackBehaviour.Pop1_pop1 => -2,
            StackBehaviour.Popi => -1,
            StackBehaviour.Popi_pop1 => -2,
            StackBehaviour.Popi_popi => -2,
            StackBehaviour.Popi_popi8 => -2,
            StackBehaviour.Popi_popi_popi => -3,
            StackBehaviour.Popi_popr4 => -2,
            StackBehaviour.Popi_popr8 => -2,
            StackBehaviour.Popref => -1,
            StackBehaviour.Popref_pop1 => -2,
            StackBehaviour.Popref_popi => -2,
            StackBehaviour.Popref_popi_popi => -3,
            StackBehaviour.Popref_popi_popi8 => -3,
            StackBehaviour.Popref_popi_popr4 => -3,
            StackBehaviour.Popref_popi_popr8 => -3,
            StackBehaviour.Popref_popi_popref => -3,
            StackBehaviour.PopAll => int.MinValue,
            StackBehaviour.Push0 => 0,
            StackBehaviour.Push1 => 1,
            StackBehaviour.Push1_push1 => 2,
            StackBehaviour.Pushi => 1,
            StackBehaviour.Pushi8 => 1,
            StackBehaviour.Pushr4 => 1,
            StackBehaviour.Pushr8 => 1,
            StackBehaviour.Pushref => 1,
            StackBehaviour.Varpop => int.MinValue,
            StackBehaviour.Varpush => int.MaxValue,
            _ => throw new ArgumentOutOfRangeException(nameof(sb), sb, $"Invalid {nameof(StackBehaviour)} enum value!"),
        };
    }

    /// <summary>
    /// Represents a possible state of the stack at a certain point in time.
    /// </summary>
    public class StackFrame {
        // AAAA THE STACK FRAME CANNOT BE IN A SUPERPOSITON, WHYYYY
        public readonly int stackAmount;

        private StackFrame(int stackAmount) {
            this.stackAmount = stackAmount;
        }

        private StackFrame() {
            this.stackAmount = -1;
        }

        public StackFrame PushType(int c) {
            return new StackFrame(stackAmount + c);
        }

        public StackFrame PopType(int c) {
            return new StackFrame(stackAmount - c);
        }

        // public StackFrame MergeWith(StackFrame other) {
        //     return new StackFrame(Merge(conditionalTypes, other.conditionalTypes));
        // }

        // public StackFrame PopConditional(int c) {
        //     return new StackFrame(baseTypes, Merge(conditionalTypes, -c));
        // }

        public static StackFrame Empty() => new(0);
        public static StackFrame Null() => new();
        
        public override string ToString() {
            return $"Amount: {stackAmount}";
        }

        public bool Equals(StackFrame obj) => this.stackAmount == obj.stackAmount;

        public bool IsNull() => stackAmount == -1;

        // private static HashSet<int> ShiftEntries(HashSet<int> entries, int c) {
        //     HashSet<int> shiftedEntries = new(entries.Count);
        //     foreach (int entry in entries) {
        //         shiftedEntries.Add(entry + c);
        //     }
        //
        //     return shiftedEntries;
        // }

        // private static HashSet<int> Merge(HashSet<int> entries, HashSet<int> otherEntries) {
        //     HashSet<int> newEntries = new(entries);
        //     newEntries.UnionWith(otherEntries);
        //     return newEntries;
        // }
    }

    // public class OpCodeAction {
    //     public int FinalState { get; private set; } = 0;
    //     public bool Jump { get; private set; } = false;
    //     public OpCodeSpecialType SpecialType { get; private set; } = OpCodeSpecialType.NoSpecial;
    //
    //     private OpCodeAction() {
    //     }
    //
    //     private OpCodeAction Push(int c) {
    //         FinalState += c;
    //         return this;
    //     }
    //
    //     private OpCodeAction Pop(int c) {
    //         FinalState -= c;
    //         return this;
    //     }
    //
    //     private OpCodeAction Jmp() {
    //         Jump = true;
    //         return this;
    //     }
    //
    //     private static OpCodeAction AutoPush(int c) {
    //         return new OpCodeAction().Push(c);
    //     }
    //
    //     private static OpCodeAction AutoPop(int c) {
    //         return new OpCodeAction().Pop(c);
    //     }
    //
    //     private static OpCodeAction OperandDependent() {
    //         return new OpCodeAction {
    //             SpecialType = OpCodeSpecialType.OperatorDependent,
    //         };
    //     }
    //     
    //     private static OpCodeAction Prefix() {
    //         return new OpCodeAction {
    //             SpecialType = OpCodeSpecialType.Prefix,
    //         };
    //     }
    //
    //     public enum OpCodeSpecialType {
    //         NoSpecial,
    //         OperatorDependent,
    //         Prefix,
    //     }
    //     
    //     public static readonly Dictionary<OpCode, OpCodeAction> OpCodeActions = new(){
    //         { OpCodes.Add, AutoPop(2).Push(1) },
    //         { OpCodes.Add_Ovf, AutoPop(2).Push(1) },
    //         { OpCodes.Add_Ovf_Un, AutoPop(2).Push(1) },
    //         { OpCodes.And, AutoPop(2).Push(1) },
    //         { OpCodes.Arglist, AutoPush(1) },
    //         { OpCodes.Beq, AutoPop(2).Jmp() },
    //         { OpCodes.Beq_S, AutoPop(2).Jmp() },
    //         { OpCodes.Bge, AutoPop(2).Jmp() },
    //         { OpCodes.Bge_S, AutoPop(2).Jmp() },
    //         { OpCodes.Bge_Un, AutoPop(2).Jmp() },
    //         { OpCodes.Bge_Un_S, AutoPop(2).Jmp() },
    //         { OpCodes.Bgt, AutoPop(2).Jmp() },
    //         { OpCodes.Bgt_S, AutoPop(2).Jmp() },
    //         { OpCodes.Bgt_Un, AutoPop(2).Jmp() },
    //         { OpCodes.Bgt_Un_S, AutoPop(2).Jmp() },
    //         { OpCodes.Ble, AutoPop(2).Jmp() },
    //         { OpCodes.Ble_S, AutoPop(2).Jmp() },
    //         { OpCodes.Ble_Un, AutoPop(2).Jmp() },
    //         { OpCodes.Ble_Un_S, AutoPop(2).Jmp() },
    //         { OpCodes.Blt, AutoPop(2).Jmp() },
    //         { OpCodes.Blt_Un, AutoPop(2).Jmp() },
    //         { OpCodes.Blt_Un_S, AutoPop(2).Jmp() },
    //         { OpCodes.Bne_Un, AutoPop(2).Jmp() },
    //         { OpCodes.Bne_Un_S, AutoPop(2).Jmp() },
    //         { OpCodes.Box, AutoPop(2).Push(1) },
    //         { OpCodes.Br, AutoPop(0).Jmp() },
    //         { OpCodes.Br_S, AutoPop(0).Jmp() },
    //         { OpCodes.Break, AutoPop(0) }, // Does literally nothing
    //         { OpCodes.Brfalse, AutoPop(1).Jmp() },
    //         { OpCodes.Brfalse_S, AutoPop(1).Jmp() },
    //         { OpCodes.Brtrue, AutoPop(1).Jmp() },
    //         { OpCodes.Brtrue_S, AutoPop(1).Jmp() },
    //         { OpCodes.Call, OperandDependent() },
    //         { OpCodes.Calli, OperandDependent() },
    //         { OpCodes.Callvirt, OperandDependent() },
    //         { OpCodes.Castclass, AutoPop(1).Push(1) },
    //         { OpCodes.Ceq, AutoPop(2).Push(1) },
    //         { OpCodes.Cgt, AutoPop(2).Push(1) },
    //         { OpCodes.Cgt_Un, AutoPop(2).Push(1) },
    //         { OpCodes.Ckfinite, AutoPop(1).Push(1) },
    //         { OpCodes.Clt, AutoPop(2).Push(1) },
    //         { OpCodes.Clt_Un, AutoPop(2).Push(1) },
    //         { OpCodes.Constrained, OperandDependent() },
    //         { OpCodes.Conv_I, AutoPop(1).Push(1) }, // This is technically false, it is OperandDependent, but we will assume the conv will succeed for now
    //         { OpCodes.Conv_I1, AutoPop(1).Push(1) }, // Read comment above
    //         { OpCodes.Conv_I2, AutoPop(1).Push(1) }, // Read comment above
    //         { OpCodes.Conv_I4, AutoPop(1).Push(1) }, // Read comment above
    //         { OpCodes.Conv_I8, AutoPop(1).Push(1) }, // Read comment above
    //         { OpCodes.Conv_Ovf_I, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_I1, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_I2, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_I4, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_I8, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_I_Un, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_I1_Un, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_I2_Un, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_I4_Un, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_I8_Un, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_U, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_U1, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_U2, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_U4, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_U8, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_U_Un, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_U1_Un, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_U2_Un, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_U4_Un, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_Ovf_U8_Un, AutoPop(1).Push(1) },
    //         { OpCodes.Conv_R_Un, AutoPop(1).Push(1) }, // Read comment of Conv_I
    //         { OpCodes.Conv_R4, AutoPop(1).Push(1) }, // Read comment of Conv_I
    //         { OpCodes.Conv_R8, AutoPop(1).Push(1) }, // Read comment of Conv_I
    //         { OpCodes.Conv_U, AutoPop(1).Push(1) }, // Read comment of Conv_I
    //         { OpCodes.Conv_U1, AutoPop(1).Push(1) }, // Read comment of Conv_I
    //         { OpCodes.Conv_U2, AutoPop(1).Push(1) }, // Read comment of Conv_I
    //         { OpCodes.Conv_U4, AutoPop(1).Push(1) }, // Read comment of Conv_I
    //         { OpCodes.Conv_U8, AutoPop(1).Push(1) }, // Read comment of Conv_I
    //         { OpCodes.Cpblk, AutoPop(3) },
    //         { OpCodes.Cpobj, AutoPop(2) },
    //         { OpCodes.Div, AutoPop(2).Push(1) },
    //         { OpCodes.Div_Un, AutoPop(2).Push(1) },
    //         { OpCodes.Dup, AutoPush(1) },
    //         { OpCodes.Endfilter, AutoPop(1) },
    //         { OpCodes.Endfinally, AutoPush(0) },
    //         { OpCodes.Initblk, AutoPop(3) },
    //         { OpCodes.Initobj, AutoPop(1).Push(1) },
    //         { OpCodes.Isinst, AutoPop(1).Push(1) },
    //         { OpCodes.Jmp, AutoPop(0).Jmp() }, // Fact: stack is empty before this
    //         { OpCodes.Ldarg, AutoPush(1) },
    //         { OpCodes.Ldarg_0, AutoPush(1) },
    //         { OpCodes.Ldarg_1, AutoPush(1) },
    //         { OpCodes.Ldarg_2, AutoPush(1) },
    //         { OpCodes.Ldarg_3, AutoPush(1) },
    //         { OpCodes.Ldarg_S, AutoPush(1) },
    //         { OpCodes.Ldarga, AutoPush(1) },
    //         { OpCodes.Ldarga_S, AutoPush(1) },
    //         { OpCodes.Ldc_I4, AutoPush(1) },
    //         { OpCodes.Ldc_I4_0, AutoPush(1) },
    //         { OpCodes.Ldc_I4_1, AutoPush(1) },
    //         { OpCodes.Ldc_I4_2, AutoPush(1) },
    //         { OpCodes.Ldc_I4_3, AutoPush(1) },
    //         { OpCodes.Ldc_I4_4, AutoPush(1) },
    //         { OpCodes.Ldc_I4_5, AutoPush(1) },
    //         { OpCodes.Ldc_I4_6, AutoPush(1) },
    //         { OpCodes.Ldc_I4_7, AutoPush(1) },
    //         { OpCodes.Ldc_I4_8, AutoPush(1) },
    //         { OpCodes.Ldc_I4_M1, AutoPush(1) },
    //         { OpCodes.Ldc_I4_S, AutoPush(1) },
    //         { OpCodes.Ldc_I8, AutoPush(1) },
    //         { OpCodes.Ldc_R4, AutoPush(1) },
    //         { OpCodes.Ldc_R8, AutoPush(1) },
    //         { OpCodes.Ldelem_Any, AutoPop(2).Push(1) },
    //         { OpCodes.Ldelem_I, AutoPop(2).Push(1) },
    //         { OpCodes.Ldelem_I1, AutoPop(2).Push(1) },
    //         { OpCodes.Ldelem_I2, AutoPop(2).Push(1) },
    //         { OpCodes.Ldelem_I4, AutoPop(2).Push(1) },
    //         { OpCodes.Ldelem_I8, AutoPop(2).Push(1) },
    //         { OpCodes.Ldelem_R4, AutoPop(2).Push(1) },
    //         { OpCodes.Ldelem_R8, AutoPop(2).Push(1) },
    //         { OpCodes.Ldelem_Ref, AutoPop(2).Push(1) },
    //         { OpCodes.Ldelem_U1, AutoPop(2).Push(1) },
    //         { OpCodes.Ldelem_U2, AutoPop(2).Push(1) },
    //         { OpCodes.Ldelem_U4, AutoPop(2).Push(1) },
    //         { OpCodes.Ldlen, AutoPop(1).Push(1) },
    //         { OpCodes.Ldloc, AutoPush(1) },
    //         { OpCodes.Ldloc_0, AutoPush(1) },
    //         { OpCodes.Ldloc_1, AutoPush(1) },
    //         { OpCodes.Ldloc_2, AutoPush(1) },
    //         { OpCodes.Ldloc_3, AutoPush(1) },
    //         { OpCodes.Ldloc_S, AutoPush(1) },
    //         { OpCodes.Ldloca, AutoPush(1) },
    //         { OpCodes.Ldloca_S, AutoPush(1) },
    //         { OpCodes.Ldnull, AutoPush(1) },
    //         { OpCodes.Ldobj, AutoPop(1).Push(1) },
    //         { OpCodes.Ldsfld, AutoPush(1) },
    //         { OpCodes.Ldsflda, AutoPush(1) },
    //         { OpCodes.Ldstr, AutoPush(1) },
    //         { OpCodes.Ldtoken, AutoPush(1) },
    //         { OpCodes.Ldvirtftn, AutoPop(1).Push(1) },
    //         { OpCodes.Leave, AutoPop(0).Jmp() },
    //         { OpCodes.Leave_S, AutoPop(0).Jmp() },
    //         { OpCodes.Localloc, AutoPop(1).Push(1) },
    //         { OpCodes.Mkrefany, AutoPop(1).Push(1) },
    //         { OpCodes.Mul, AutoPop(2).Push(1) },
    //         { OpCodes.Mul_Ovf, AutoPop(2).Push(1) },
    //         { OpCodes.Mul_Ovf_Un, AutoPop(2).Push(1) },
    //         { OpCodes.Neg, AutoPop(1).Push(1) },
    //         { OpCodes.Newarr, AutoPop(1).Push(1) },
    //         { OpCodes.Newobj, OperandDependent() },
    //         { OpCodes.No, Prefix() },
    //         { OpCodes.Nop, AutoPop(0) },
    //         { OpCodes.Not, AutoPop(1).Push(1) },
    //         { OpCodes.Or, AutoPop(2).Push(1) },
    //         { OpCodes.Pop, AutoPop(1) },
    //         // The following opcodes are missing from Cecil
    //         /* { OpCodes.Prefix1, AutoPop(0) }, */
    //         /* { OpCodes.Prefix2, AutoPop(0) }, */
    //         /* { OpCodes.Prefix3, AutoPop(0) }, */
    //         /* { OpCodes.Prefix4, AutoPop(0) }, */
    //         /* { OpCodes.Prefix5, AutoPop(0) }, */
    //         /* { OpCodes.Prefix6, AutoPop(0) }, */
    //         /* { OpCodes.Prefix7, AutoPop(0) }, */
    //         /* { OpCodes.Prefixref, AutoPop(0) }, */
    //         { OpCodes.Readonly, OperandDependent() },
    //         { OpCodes.Refanytype, AutoPop(1).Push(1) },
    //         { OpCodes.Refanyval, AutoPop(1).Push(1) },
    //         { OpCodes.Rem, AutoPop(2).Push(1) },
    //         { OpCodes.Rem_Un, AutoPop(2).Push(1) },
    //         { OpCodes.Ret, OperandDependent() }, // Ret is not operand dependent, but rather method dependent
    //         { OpCodes.Rethrow, AutoPop(0) },
    //         { OpCodes.Shl, AutoPop(2).Push(1) },
    //         { OpCodes.Shr, AutoPop(2).Push(1) },
    //         { OpCodes.Shr_Un, AutoPop(2).Push(1) },
    //         { OpCodes.Sizeof, AutoPush(1) },
    //         { OpCodes.Starg, AutoPop(1) },
    //         { OpCodes.Starg_S, AutoPop(1) },
    //         { OpCodes.Stelem_Any, AutoPop(3) },
    //         { OpCodes.Stelem_I, AutoPop(3) },
    //         { OpCodes.Stelem_I1, AutoPop(3) },
    //         { OpCodes.Stelem_I2, AutoPop(3) },
    //         { OpCodes.Stelem_I4, AutoPop(3) },
    //         { OpCodes.Stelem_I8, AutoPop(3) },
    //         { OpCodes.Stelem_R4, AutoPop(3) },
    //         { OpCodes.Stelem_R8, AutoPop(3) },
    //         { OpCodes.Stelem_Ref, AutoPop(3) },
    //         { OpCodes.Stfld, AutoPop(2) },
    //         { OpCodes.Stind_I, AutoPop(2) },
    //         { OpCodes.Stind_I1, AutoPop(2) },
    //         { OpCodes.Stind_I2, AutoPop(2) },
    //         { OpCodes.Stind_I4, AutoPop(2) },
    //         { OpCodes.Stind_I8, AutoPop(2) },
    //         { OpCodes.Stind_R4, AutoPop(2) },
    //         { OpCodes.Stind_R8, AutoPop(2) },
    //         { OpCodes.Stind_Ref, AutoPop(3) },
    //         { OpCodes.Stloc, AutoPop(1) },
    //         { OpCodes.Stloc_0, AutoPop(1) },
    //         { OpCodes.Stloc_1, AutoPop(1) },
    //         { OpCodes.Stloc_2, AutoPop(1) },
    //         { OpCodes.Stloc_3, AutoPop(1) },
    //         { OpCodes.Stloc_S, AutoPop(1) },
    //         { OpCodes.Stobj, AutoPop(2) },
    //         { OpCodes.Stsfld, AutoPop(1) },
    //         { OpCodes.Sub, AutoPop(2).Push(1) },
    //         { OpCodes.Sub_Ovf, AutoPop(2).Push(1) },
    //         { OpCodes.Sub_Ovf_Un, AutoPop(2).Push(1) },
    //         { OpCodes.Switch, AutoPop(1) }, // Switch is actually surprisingly simple for what we care :D
    //         { OpCodes.Tail, OperandDependent() },
    //         { OpCodes.Throw, AutoPop(1) },
    //         { OpCodes.Unaligned, OperandDependent() },
    //         { OpCodes.Unbox, AutoPop(1).Push(1) },
    //         { OpCodes.Unbox_Any, AutoPop(1).Push(1) },
    //         { OpCodes.Volatile, OperandDependent() },
    //         { OpCodes.Xor, AutoPop(2).Push(1) },
    //     };
}