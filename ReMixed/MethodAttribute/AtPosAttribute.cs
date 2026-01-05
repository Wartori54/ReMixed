using System;

namespace ReMixed.MethodAttribute;

[AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
public class AtPosAttribute(string id, string targetEl) : Attribute {
    // Pairing Id
    public string Id { get; set; } = id;
    // Target element for the positioner
    public string TargetEl { get; set; } = targetEl;
}