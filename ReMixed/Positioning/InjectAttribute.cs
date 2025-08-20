// Cleanup needed

using System;
using ReMixed.MethodAttribute;

namespace ReMixed.Positioning;

[Obsolete]
[AttributeUsage(AttributeTargets.Method, Inherited = false, AllowMultiple = true)]
public class InjectAttribute(string methodTarget, bool cancellable = false, InjectLocation.Shift shift = InjectLocation.Shift.BeforeArguments, int index = 0) : MethodPositionedAttribute(methodTarget, [""]) {
    public bool Cancellable { get; } = cancellable;
    public InjectLocation.Shift Shift { get; } = shift;
    public int Index { get; } = index;
}