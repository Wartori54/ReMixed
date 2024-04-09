using Mono.Cecil;

namespace ReMixed;

public static class Typing {
    public static string FullTypeBasicTypes(string type) {
        return type switch {
            "string" => "System.String",
            "bool" => "System.Boolean",
            "byte" => "System.Byte",
            "sbyte" => "System.SByte",
            "char" => "System.Char",
            "short" => "System.Int16",
            "ushort" => "System.UInt16",
            "int" => "System.Int32",
            "uint" => "System.UInt32",
            "long" => "System.Int64",
            "ulong" => "System.uInt64",
            "float" => "System.Single",
            "double" => "System.Double",
            "decimal" => "System.Decimal",
            "nint" => "System.IntPtr",
            "nuint" => "System.UIntPtr",
            "void" => "System.Void",
            "object" => "System.Object",
            _ => type,
        };
    }

    public static string GetGenericSafeTypeName(GenericInstanceType typeReference) {
        if (!typeReference.HasGenericArguments) return typeReference.Name;
        return typeReference.Name.Split('`')[0];
    }
}
