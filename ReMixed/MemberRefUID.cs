using System;
using Mono.Cecil;
// ReSharper disable MemberCanBePrivate.Global

namespace ReMixed;

// CLS compliant representation of references that guarantee uniqueness.

// DeclaringType is never generic due to it actually being the same member if it were to be a generic instance type
// Due to the member being defined from the generic un-parametrized type
// This is not true for the other type references

// TODO: Cache hashes because everything is RO and these structs are meant to be used in dicts
public readonly record struct FieldRefUID {
    
    public readonly string Name;
    public readonly TypeRefUID DeclaringType;

    // CLS does not allow field overloading, this is extra
    public readonly GTypeRefUID FieldType;
    
    internal FieldRefUID(string name, TypeRefUID declaringType, GTypeRefUID fieldType) {
        Name = name;
        DeclaringType = declaringType;
        FieldType = fieldType;
    }
}

public readonly record struct MethodParamUID {
    public readonly GTypeRefUID ParamType;
    public readonly ParameterAttributes Attributes;
    
    public MethodParamUID(GTypeRefUID paramType, ParameterAttributes attributes) {
        ParamType = paramType;
        Attributes = attributes;
    }
}

public readonly record struct MethodRefUID {
    public readonly string Name;
    public readonly TypeRefUID DeclaringType;
    
    // CLS does not allow return type overloading, except for a very specific exception, thus handle it in general for ease of use
    public readonly GTypeRefUID ReturnType;
    // ReSharper disable once TypeWithSuspiciousEqualityIsUsedInRecord.Global
    public readonly MethodParamUID[] Parameters; // Parameters can differ only by the attributes (being byref or not) so the types are not enough here
    private readonly int fullHash;
    
    public MethodRefUID(string name, TypeRefUID declaringType, GTypeRefUID returnType, MethodParamUID[] parameters) {
        Name = name;
        DeclaringType = declaringType;
        ReturnType = returnType;
        Parameters = parameters;
        fullHash = GetHashCodeImpl();
    }

    // Calling conv, generic params, has_this and explicit_this not required since we already have uniqueness guaranteed with the above
    public bool Equals(MethodRefUID other) {
        bool part1 = Name == other.Name && DeclaringType.Equals(other.DeclaringType) && ReturnType.Equals(other.ReturnType) 
                     && Parameters.Length == other.Parameters.Length;
        if (!part1) return false;
        for (int i = 0; i < Parameters.Length; i++) {
            if (!Parameters[i].Equals(other.Parameters[i])) return false;
        }
        return true;
    }

    public int GetHashCodeImpl() {
        return HashCode.Combine(Name, DeclaringType, ReturnType, Util.HashArrContents(Parameters));
    }

    public override int GetHashCode() {
        return fullHash;
    }
}

public readonly record struct PropertyRefUID {
    public readonly string Name;
    public readonly TypeRefUID DeclaringType;

    public readonly GTypeRefUID PropertyType;

    public PropertyRefUID(string name, TypeRefUID declaringType, GTypeRefUID propertyType) {
        Name = name;
        DeclaringType = declaringType;
        PropertyType = propertyType;
    }
}

public readonly record struct EventRefUID {
    public readonly string Name;
    public readonly TypeRefUID DeclaringType;
    
    public readonly GTypeRefUID EventType;
    
    public EventRefUID(string name, TypeRefUID declaringType, GTypeRefUID eventType) {
        Name = name;
        DeclaringType = declaringType;
        EventType = eventType;
    }
}

public readonly record struct TypeRefUID {
    public readonly string Name;
    public readonly TypeRefOptional? DeclaringType;

    public readonly string Namespace;
    public readonly ModuleDefinition Module;
    
    public TypeRefUID(string name, string ns, TypeRefOptional? declaringType, ModuleDefinition module) {
        Name = name;
        Namespace = ns;
        DeclaringType = declaringType;
        Module = module;
    }
    
    public override int GetHashCode() {
        return DeclaringType == null ? 
            HashCode.Combine(Name, Namespace, Module) : 
            HashCode.Combine(Name, DeclaringType.Value,  Namespace, Module);
    }

    public bool Equals(TypeRefUID other) {
        if ((DeclaringType == null) != (other.DeclaringType == null)) return false;
        bool part1 = Name == other.Name && Namespace == other.Namespace && Module == other.Module;
        if (DeclaringType != null) {
            part1 = part1 && DeclaringType.Value.Equals(other.DeclaringType!.Value);
        }
        return part1;
    }
}

public readonly record struct GTypeRefUID {
    public readonly string Name;
    public readonly TypeRefOptional? DeclaringType;

    public readonly string Namespace;
    public readonly ModuleDefinition Module;
    
    // ReSharper disable once TypeWithSuspiciousEqualityIsUsedInRecord.Global
    public readonly GTypeRefUID[] GenericParameters;
    private readonly int fullHash;
    public GTypeRefUID(string name, string ns, TypeRefOptional? declaringType, ModuleDefinition module, GTypeRefUID[] genericParameters) {
        Name = name;
        DeclaringType = declaringType;
        Namespace = ns;
        Module = module;
        GenericParameters = genericParameters;
        fullHash = GetHashCodeImpl();
    }

    private int GetHashCodeImpl() {
        return DeclaringType == null ? HashCode.Combine(Name, Namespace, Module, Util.HashArrContents(GenericParameters)) : HashCode.Combine(Name, DeclaringType.Value, Namespace, Module, Util.HashArrContents(GenericParameters));
    }

    public override int GetHashCode() {
        return fullHash;
    }

    public bool Equals(GTypeRefUID other) {
        if ((DeclaringType == null) != (other.DeclaringType == null)) return false;
        bool part1 = Name == other.Name && Namespace == other.Namespace && Module == other.Module && GenericParameters.Length == other.GenericParameters.Length;
        if (DeclaringType != null) {
            part1 = part1 && DeclaringType.Value.Equals(other.DeclaringType!.Value);
        }
        if (!part1) return false;
        for (int i = 0; i < GenericParameters.Length; i++) {
            if (!GenericParameters[i].Equals(other.GenericParameters[i])) return false;
        }
        return true;
    }
}

public class TypeRefOptional(TypeRefUID value) {
    public readonly TypeRefUID Value = value;

    public override int GetHashCode() {
        throw new NotSupportedException();
    }

    public override bool Equals(object? obj) {
        throw new NotSupportedException();
    }
}

public static class MemberRefUIDGen {
    public static TypeRefUID ToUID(this TypeReference value, bool disableGenericCheck = false) {
        if (!disableGenericCheck && value is GenericInstanceType) {
            throw new ArgumentException("Tried to get UID of generic type reference without opting out", nameof(value));
        }
        ArgumentNullException.ThrowIfNull(value.Module, nameof(value) + "." + nameof(value.Module));
        return new TypeRefUID(value.Name, value.Namespace, value.DeclaringType != null ? new(value.DeclaringType.ToUID()) : null, value.Module);
    }

    public static GTypeRefUID ToGenericUID(this TypeReference value) {
        ArgumentNullException.ThrowIfNull(value.Module, nameof(value) + "." + nameof(value.Module));
        GTypeRefUID[] gParams;
        if (value is GenericInstanceType git) {
            gParams = new GTypeRefUID[git.GenericArguments.Count];
            for (int i = 0; i < git.GenericArguments.Count; i++) {
                gParams[i] = ToGenericUID(git.GenericArguments[i]);
            }
        } else {
            gParams = [];
        }
        return new GTypeRefUID(value.Name, value.Namespace, value.DeclaringType != null ? new(value.DeclaringType.ToUID()) : null, value.Module, gParams);
    }

    // Intentionally ignore generic args from declaring types on all refs since it's relinkage does not depend on those
    public static FieldRefUID ToUID(this FieldReference value) {
        ArgumentNullException.ThrowIfNull(value.DeclaringType, nameof(value.DeclaringType) + "." + nameof(value.DeclaringType));
        return new FieldRefUID(value.Name, value.DeclaringType.ToUID(true), value.FieldType.ToGenericUID());
    }

    public static MethodRefUID ToUID(this MethodReference value) {
        ArgumentNullException.ThrowIfNull(value.DeclaringType, nameof(value.DeclaringType) + "." + nameof(value.DeclaringType));
        MethodParamUID[] parameters = new MethodParamUID[value.Parameters.Count];
        for (int i = 0; i < value.Parameters.Count; i++) {
            parameters[i] = new MethodParamUID(value.Parameters[i].ParameterType.ToGenericUID(), value.Parameters[i].Attributes);
        }
        return new MethodRefUID(value.Name, value.DeclaringType.ToUID(true), value.ReturnType.ToGenericUID(), parameters);
    }

    public static PropertyRefUID ToUID(this PropertyReference value) {
        ArgumentNullException.ThrowIfNull(value.DeclaringType, nameof(value.DeclaringType) + "." + nameof(value.DeclaringType));
        return new PropertyRefUID(value.Name, value.DeclaringType.ToUID(true), value.PropertyType.ToGenericUID());
    }

    public static EventRefUID ToUID(this EventReference value) {
        ArgumentNullException.ThrowIfNull(value.DeclaringType, nameof(value.DeclaringType) + "." + nameof(value.DeclaringType));
        return new EventRefUID(value.Name, value.DeclaringType.ToUID(), value.EventType.ToGenericUID());
    }
}
