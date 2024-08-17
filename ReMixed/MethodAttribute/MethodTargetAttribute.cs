using System;

namespace ReMixed.MethodAttribute;

/// <summary>
/// Declares a patching relationship with another method, methodTarget may be a fully qualified name or just a name.
/// </summary>
/// <param name="methodTarget">The name.</param>
/// <remarks>It is mandatory for subclasses to have this argument as the first in the constructor.</remarks>
[AttributeUsage(AttributeTargets.Method, Inherited = true, AllowMultiple = true)]
public class MethodTargetAttribute(string methodTarget) : System.Attribute {
    // Target name for the injection
    public string MethodTarget { get; } = methodTarget;
}