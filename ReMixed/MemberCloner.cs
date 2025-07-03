using System;
using System.Collections.Generic;
using System.Diagnostics.Tracing;
using Mono.Cecil;
using Mono.Cecil.Cil;

namespace ReMixed;

public static class MemberCloner {
    public static FieldDefinition Clone(this FieldDefinition source) {
        return new FieldDefinition(source.Name, source.Attributes, source.FieldType) {
            IsSpecialName = source.IsSpecialName,
            IsRuntimeSpecialName = source.IsRuntimeSpecialName,
        }.CloneCustomAttributes(source);
    }

    public static PropertyDefinition Clone(this PropertyDefinition source) {
        PropertyDefinition copy = new(source.Name, source.Attributes, source.PropertyType) {
            IsSpecialName = source.IsSpecialName,
            IsRuntimeSpecialName = source.IsRuntimeSpecialName,
            GetMethod = source.GetMethod,
            SetMethod = source.SetMethod,
        };
        foreach (MethodDefinition m in source.OtherMethods) {
            copy.OtherMethods.Add(m);
        }
        return copy.CloneCustomAttributes(source);
    }

    public static EventDefinition Clone(this EventDefinition source) {
        EventDefinition copy = new(source.Name, source.Attributes, source.EventType) {
            IsSpecialName = source.IsSpecialName,
            IsRuntimeSpecialName = source.IsRuntimeSpecialName,
            AddMethod = source.AddMethod,
            RemoveMethod = source.RemoveMethod,
            InvokeMethod = source.InvokeMethod,
        };
        foreach (MethodDefinition m in source.OtherMethods) {
            copy.OtherMethods.Add(m);
        }
        return copy.CloneCustomAttributes(source);
    }

    public static MethodDefinition Clone(this MethodDefinition source) {
        MethodDefinition copy = new(source.Name, source.Attributes, source.ReturnType) {
            IsSpecialName = source.IsSpecialName,
            IsRuntimeSpecialName = source.IsRuntimeSpecialName,
            MethodReturnType = source.MethodReturnType,
            // Body = source.Body.Clone(),
            CallingConvention = source.CallingConvention,
            ImplAttributes = source.ImplAttributes,
            DebugInformation = source.DebugInformation/*.Clone()*/, // TODO
            SemanticsAttributes = source.SemanticsAttributes,
        };
        // The setter of this adds the IsPInvoke attribute regardless of whether this is null nor not
        // So do not assign unless there's something to deal with
        if (source.PInvokeInfo != null) { 
            copy.PInvokeInfo = source.PInvokeInfo;
        }
        
        foreach (MethodReference @override in source.Overrides) {
            copy.Overrides.Add(@override);
        }
        
        foreach (ParameterDefinition parameter in source.Parameters) {
            copy.Parameters.Add(parameter.Clone());
        }
        
        foreach (GenericParameter genericParameter in source.GenericParameters) {
            copy.GenericParameters.Add(genericParameter.Clone(copy, pair: false));
        }
        
        foreach (CustomDebugInformation customDebugInformation in source.CustomDebugInformations) {
            copy.CustomDebugInformations.Add(customDebugInformation);
        }
        
        copy.Body = source.Body.Clone(copy);
        // TODO
        // foreach (SecurityDeclaration securityDeclaration in source.SecurityDeclarations) {
        //     copy.SecurityDeclarations.Add(securityDeclaration);
        // }

        return copy.CloneCustomAttributes(source);
    }

    public static MethodBody Clone(this MethodBody source, MethodDefinition owner) {
        MethodBody copy = new(owner) {
            InitLocals = source.InitLocals,
            MaxStackSize = source.MaxStackSize,
            
            // ThisParameter is already handled by cecil itself
            /*LocalVarToken = source.LocalVarToken,*/ // Dont copy this
        };
        
        // Even though `varDef.Clone()` doesn't assign the index, adding it here will
        // thus it will match the original ones
        foreach (VariableDefinition variable in source.Variables) {
            copy.Variables.Add(variable.Clone());
        }

        Dictionary<Instruction, Action<Instruction>> exhHandlerInstrReplacer = new();
        foreach (ExceptionHandler exceptionHandler in source.ExceptionHandlers) {
            ExceptionHandler newExhHandler = new(exceptionHandler.HandlerType);
            copy.ExceptionHandlers.Add(newExhHandler);
            
            if (exceptionHandler.TryStart != null)
                exhHandlerInstrReplacer[exceptionHandler.TryStart] = i => newExhHandler.TryStart = i;
            if (exceptionHandler.TryEnd != null)
                exhHandlerInstrReplacer[exceptionHandler.TryEnd] = i => newExhHandler.TryEnd = i;
            if (exceptionHandler.FilterStart != null)
                exhHandlerInstrReplacer[exceptionHandler.FilterStart] = i => newExhHandler.FilterStart = i;
            if (exceptionHandler.HandlerStart != null)
                exhHandlerInstrReplacer[exceptionHandler.HandlerStart] = i => newExhHandler.HandlerStart = i;
            if (exceptionHandler.HandlerEnd != null)
                exhHandlerInstrReplacer[exceptionHandler.HandlerEnd] = i => newExhHandler.HandlerEnd = i;
            
            newExhHandler.CatchType = exceptionHandler.CatchType;
        }

        List<(int ciidx, Instruction targetInstr)> instrsToReplace = [];
        List<(List<(int ciidx, int idx)>, Instruction targetInstr)> instrsListsToReplace = [];
        int iidx = 0;
        foreach (Instruction instruction in source.Instructions) {
            // Cecil wants to prevent creating instructions with the wrong operand
            // But code is jank that way, so trick it into giving us an instr and then just overwrite it entirely
            Instruction newInstr = Instruction.Create(OpCodes.Nop);
            newInstr.OpCode = instruction.OpCode;
            newInstr.Offset = instruction.Offset;
            object? newOperand;
            switch (instruction.Operand) {
                case string:
                case sbyte:
                case byte:
                case int:
                case long:
                case float:
                case double:
                case null:
                    newOperand = instruction.Operand;
                    break;
                case VariableReference v:
                    newOperand = copy.Variables[v.Index];
                    break;
                case ParameterReference p:
                    newOperand = copy.Method.Parameters[p.Index];
                    break;
                case Instruction i: {
                    int ciidx = source.Instructions.IndexOf(i);
                    if (ciidx < iidx) {
                        newOperand = copy.Instructions[ciidx];
                    } else {
                        instrsToReplace.Add((ciidx, newInstr));
                        newOperand = null;
                    }
                    break;
                }
                case Instruction[] iis: {
                    Instruction[] newInstrs = new Instruction[iis.Length];
                    List<(int ciidx, int idx)> rps = [];
                    for (int idx = 0; idx < iis.Length; idx++) {
                        Instruction i = iis[idx];
                        int ciidx = source.Instructions.IndexOf(i);
                        if (ciidx < iidx) {
                            newInstrs[idx] = copy.Instructions[ciidx];
                        } else {
                            rps.Add((ciidx, idx));
                        }
                    }
                    if (rps.Count != 0)
                        instrsListsToReplace.Add((rps, newInstr));
                    newOperand = newInstrs;
                    break;
                }
                case TypeReference: // All of these will be relinked in the later pass
                case FieldReference:
                case MethodReference:
                case CallSite:
                    newOperand = instruction.Operand;
                    break;
                default:
                    throw new NotSupportedException("Unknown operand of type: " + instruction.Operand.GetType());
            }
            
            newInstr.Operand = newOperand;
            if (exhHandlerInstrReplacer.TryGetValue(instruction, out Action<Instruction>? handler)) {
                handler.Invoke(newInstr);
            }
            copy.Instructions.Add(newInstr);
            iidx++;
        }

        foreach ((int ciidx, Instruction targetInstr) in instrsToReplace) {
            targetInstr.Operand = copy.Instructions[ciidx];
        }

        foreach ((List<(int ciidx, int idx)>? valueTuples, Instruction targetInstr) in instrsListsToReplace) {
            Instruction[] op = targetInstr.Operand as Instruction[] ?? throw new InvalidOperationException();
            foreach ((int ciidx, int idx) in valueTuples) {
                op[idx] = copy.Instructions[ciidx];
            }
        }

        return copy;
    }

    public static VariableDefinition Clone(this VariableDefinition source) {
        return new VariableDefinition(source.VariableType);
    }

    public static ParameterDefinition Clone(this ParameterDefinition source) {
        return new ParameterDefinition(source.Name, source.Attributes, source.ParameterType).CloneCustomAttributes(source);
    }

    public static InterfaceImplementation Clone(this InterfaceImplementation source) {
        return new InterfaceImplementation(source.InterfaceType).CloneCustomAttributes(source);
    }

    public static GenericParameter Clone(this GenericParameter source, IGenericParameterProvider dest, bool pair = true) {
        if (pair && source.Position < dest.GenericParameters.Count) {
            return dest.GenericParameters[source.Position];
        }
        GenericParameter copy = new(source.Name, dest) {
            Attributes = source.Attributes,
        };

        foreach (GenericParameterConstraint constraint in source.Constraints) {
            copy.Constraints.Add(new GenericParameterConstraint(constraint.ConstraintType).CloneCustomAttributes(constraint));
        }
        
        return copy.CloneCustomAttributes(source);
    }

    public static T CloneCustomAttributes<T>(this T dest, T source) where T : ICustomAttributeProvider {
        foreach (CustomAttribute customAttribute in source.CustomAttributes) {
            dest.CustomAttributes.Add(customAttribute.Clone());
        }
        return dest;
    }

    // TODO: This is not good enough...
    public static CustomAttribute Clone(this CustomAttribute source) {
        CustomAttribute copy = new CustomAttribute(source.Constructor);
        foreach (CustomAttributeArgument customAttributeArgument in source.ConstructorArguments) {
            copy.ConstructorArguments.Add(customAttributeArgument);
        }
        foreach (CustomAttributeNamedArgument field in source.Fields) {
            copy.Fields.Add(field);
        }
        foreach (CustomAttributeNamedArgument property in source.Properties) {
            copy.Properties.Add(property);
        }

        return copy;
    }


    public static void ForEachMember(this TypeDefinition source, Action<IMemberDefinition> action) {
        foreach (FieldDefinition field in source.Fields) {
            action(field);
        }
        
        foreach (PropertyDefinition property in source.Properties) {
            action(property);
        }
        
        foreach (EventDefinition @event in source.Events) {
            action(@event);
        }
        
        foreach (TypeDefinition typeDefinition in source.NestedTypes) {
            action(typeDefinition);
        }

        foreach (MethodDefinition method in source.Methods) {
            action(method);
        }

        // foreach (CustomAttribute customAttribute in source.CustomAttributes) {
        //     
        // }
        //
        // foreach (InterfaceImplementation interfaceImplementation in source.Interfaces) {
        //     
        // }
        //
        // foreach (GenericParameter genericParameter in source.GenericParameters) {
        //     
        // }
        //
        // foreach (SecurityDeclaration securityDeclaration in source.SecurityDeclarations) {
        //     
        // }
    }

    public static bool GenericEquals(this GenericParameter source, GenericParameter other) {
        if (source.Name != other.Name)
            return false;

        if (source.Type != other.Type) 
            return false;

        if (source.Attributes != other.Attributes) 
            return false;
        
        if (source.Position != other.Position) 
            return false;

        if (source.Constraints.Count != other.Constraints.Count) return false;

        for (int i = 0; i < source.Constraints.Count; i++) {
            if (!source.Constraints[i].ConstraintEquals(other.Constraints[i])) return false;
        }

        // Ignore attributes intentionally
        return true;
    }

    public static bool ConstraintEquals(this GenericParameterConstraint source, GenericParameterConstraint other) {
        // Ignore attributes intentionally
        return ILPatcher.TypeReferenceEqual(source.ConstraintType, other.ConstraintType);
    }
}