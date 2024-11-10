using System;

namespace ReMixed;

[AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, AllowMultiple = false, Inherited = false)]
public class MixinAttribute(Type target) : Attribute {
    public Type Target { get; } = target;
}