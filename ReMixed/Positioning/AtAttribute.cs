using System;

namespace ReMixed.Positioning;

[AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
public class AtAttribute(
    // Positioner used
    string value
) : Attribute {
    public string Value { get; } = value;
    // Data for the Positioner
    public string Target { get; set; } = "";
    // Shifting
    public InjectLocation.Shift Shift { get; set; } = InjectLocation.Shift.Before;
    // Entry in the valid positions
    public int Ordinal { get; set; } = -1;
    // Id for pairing
    public string Id { get; set; } = "";
};
