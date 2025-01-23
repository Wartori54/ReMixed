using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Mono.Cecil;
using Mono.Cecil.Cil;
using MonoMod.Utils;

namespace ReMixed;

public static class MemberCloner {
    public static FieldDefinition Clone(this FieldDefinition source) {
        // TODO: Copy CustomAttributes
        return new FieldDefinition(source.Name, source.Attributes, source.FieldType) {
            IsSpecialName = source.IsSpecialName,
            IsRuntimeSpecialName = source.IsRuntimeSpecialName
        };
    }

    public static PropertyDefinition Clone(this PropertyDefinition source) {
        // TODO: Copy CustomAttributes
        PropertyDefinition copy = new(source.Name, source.Attributes, source.PropertyType) {
            IsSpecialName = source.IsSpecialName,
            IsRuntimeSpecialName = source.IsRuntimeSpecialName,
            GetMethod = source.GetMethod,
            SetMethod = source.SetMethod,
        };
        foreach (MethodDefinition m in source.OtherMethods) {
            copy.OtherMethods.Add(m);
        }
        return copy;
    }

    public static EventDefinition Clone(this EventDefinition source) {
        // TODO: Copy CustomAttributes
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
        return copy;
    }

    public static MethodDefinition Clone(this MethodDefinition source) {
        // TODO: Copy CustomAttributes
        MethodDefinition copy = new(source.Name, source.Attributes, source.ReturnType) {
            IsSpecialName = source.IsSpecialName,
            IsRuntimeSpecialName = source.IsRuntimeSpecialName,
            MethodReturnType = source.MethodReturnType,
            Body = source.Body.Clone(),
            CallingConvention = source.CallingConvention,
            ImplAttributes = source.ImplAttributes,
            DebugInformation = source.DebugInformation/*.Clone()*/, // TODO
            SemanticsAttributes = source.SemanticsAttributes,
            PInvokeInfo = source.PInvokeInfo,
        };
        
        foreach (MethodReference @override in source.Overrides) {
            copy.Overrides.Add(@override);
        }
        
        foreach (ParameterDefinition parameter in source.Parameters) {
            copy.Parameters.Add(parameter);
        }
        
        foreach (GenericParameter genericParameter in source.GenericParameters) {
            copy.GenericParameters.Add(genericParameter);
        }
        
        foreach (CustomDebugInformation customDebugInformation in source.CustomDebugInformations) {
            copy.CustomDebugInformations.Add(customDebugInformation);
        }
        // TODO
        // foreach (SecurityDeclaration securityDeclaration in source.SecurityDeclarations) {
        //     copy.SecurityDeclarations.Add(securityDeclaration);
        // }

        return copy;
    }

    public static MethodBody Clone(this MethodBody source, MethodDefinition owner) {
        MethodBody copy = new(owner) {
            InitLocals = source.InitLocals,
            MaxStackSize = source.MaxStackSize,
            
            /*LocalVarToken = source.LocalVarToken,*/ // Dont copy this
        };
        
        foreach (VariableDefinition variable in source.Variables) {
            copy.Variables.Add(variable);
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

        foreach (Instruction instruction in source.Instructions) {
            Instruction newInstr = Instruction.Create(instruction.OpCode);
            newInstr.Offset = instruction.Offset;
            newInstr.Operand = instruction.Operand;
            if (exhHandlerInstrReplacer.TryGetValue(instruction, out Action<Instruction>? handler)) {
                handler.Invoke(newInstr);
            }
            copy.Instructions.Add(newInstr);
        }
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