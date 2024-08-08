using Mono.Cecil;

namespace ReMixed;

public class StackType {
    public StackTypeKind Kind { get; }
    public bool Subtype { get; private set; }

    public StackType(StackTypeKind kind, bool subtype = false) {
        Kind = kind;
        Subtype = subtype;
    }
    
    // According to "I.12.3.2.1" in the ECMA spec
    public enum StackTypeKind {
        Unknown,
        Int32,
        Int64,
        NInt,
        Float,
        ObjRef,
        MngRef,
        ValueType
    }
}

public class ObjectReferenceStackType : StackType {
    public TypeReference TypeReference { get; }

    public ObjectReferenceStackType(TypeReference typeReference) : base(StackTypeKind.ObjRef, true) {
        TypeReference = typeReference;
    }
}

public class MethodReferenceStackType : StackType {
    public MethodReference MethodReference { get; }

    // Method references are always pointers (ldftn)
    public MethodReferenceStackType(MethodReference methodReference) : base(StackTypeKind.NInt, true) {
        MethodReference = methodReference;
    }
}

public class StructVTStackType : StackType {
    public TypeReference TypeReference { get; }

    public StructVTStackType(TypeReference typeReference) : base(StackTypeKind.ValueType, true) {
        TypeReference = typeReference;
    }
}
