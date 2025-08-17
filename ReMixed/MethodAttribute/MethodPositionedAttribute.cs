using System;

namespace ReMixed.MethodAttribute;

/// <summary>
/// Declares a patching relationship with another method, methodTarget may be a fully qualified name or just a name.
/// </summary>
/// <param name="methodTarget">The name.</param>
/// <remarks>It is mandatory for subclasses to have this argument as the first in the constructor.</remarks>
[AttributeUsage(AttributeTargets.Method, Inherited = false, AllowMultiple = true)]
public class MethodPositionedAttribute(string methodTarget, 
    string[]? at) : Attribute {
    // Target name for the injection
    public string MethodTarget { get; } = methodTarget;
    
    // Ids of Ats to pair properly
    public string[] At { get; } = at ?? [""];
    // TODO: Slice
}