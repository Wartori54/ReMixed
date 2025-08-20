using ReMixed.Positioning;

namespace ReMixed.MethodAttribute;
// TODO: Local captures

public sealed class InjectAttribute : MethodPositionedAttribute {
    public InjectAttribute(
        string targetName, 
        // Ids of Ats to pair properly
        string[] at) : base(targetName, at) {
    }

    public InjectAttribute(string[] at) : base(null, at) {
    }
    // Cancellability
    public bool Cancellable { get; }
    
    // TODO: id, target, slice, locals...
}