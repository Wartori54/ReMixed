using System;
using ReMixed.MethodAttribute;

namespace ReMixed.Positioning;

[AttributeUsage(AttributeTargets.Method, Inherited = false, AllowMultiple = false)]
public class InjectAttribute(string methodTarget, bool cancellable = false, InjectLocation.Shift shift = InjectLocation.Shift.BeforeArguments, int index = 0) : MethodTargetAttribute(methodTarget) {
    public bool Cancellable { get; } = cancellable;
    public InjectLocation.Shift Shift { get; } = shift;
    public int Index { get; } = index;
}