using System;
using ReMixed.Positioning;

namespace ReMixed.Registry;

public class InjectorMPAAttribute(Type _MPAType) : Attribute {
    public Type MPAType { get; } = _MPAType;

}