using System;
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
        EventDefinition copy = new EventDefinition(source.Name, source.Attributes, source.EventType) {
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
        MethodDefinition copy = new MethodDefinition(source.Name, source.Attributes, source.ReturnType) {
            IsSpecialName = source.IsSpecialName,
            IsRuntimeSpecialName = source.IsRuntimeSpecialName,
            MethodReturnType = source.MethodReturnType,
            Body = source.Body.Clone(),
            CallingConvention = source.CallingConvention,
            ImplAttributes = source.ImplAttributes,
            DebugInformation = source.DebugInformation.Clone(),
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
            copy.CustomDebugInformations.Add(customDebugInformation.Clone());
        }
        // TODO
        // foreach (SecurityDeclaration securityDeclaration in source.SecurityDeclarations) {
        //     copy.SecurityDeclarations.Add(securityDeclaration);
        // }
        
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