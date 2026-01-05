using System;
using ReMixed.Positioning;

namespace ReMixed.MethodAttribute;
// TODO: Local captures

[AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
public sealed class InjectAttribute : MethodPositionedAttribute {
    public InjectAttribute(
        string targetName, 
        // Ids of Ats to pair properly
        string[] at) : base(targetName, at) {
    }

    public InjectAttribute(string[] at) : base(null, at) {
    }
    
    // Cancellability
    public bool Cancellable { get; } = false;
    
    public Shift Shift { get; } = Shift.None;

    public int ShiftBy { get; } = 0;

    // TODO: id, target, slice, locals...
}

public enum Shift {
    None,
    Before,
    After,
    By
}