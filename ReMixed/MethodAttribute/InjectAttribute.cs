using ReMixed.Positioning;

namespace ReMixed.MethodAttribute;

public class InjectAttribute(
    // Target name for the injection
    string targetName, 
    // Ids of Ats to pair properly
    string[]? at = null, 
    // Cancellability
    bool cancellable = false) : MethodPositionedAttribute(targetName, at) {
    public bool Cancellable { get; } = cancellable;
}