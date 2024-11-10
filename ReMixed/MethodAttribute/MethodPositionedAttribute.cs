namespace ReMixed.MethodAttribute;

public class MethodPositionedAttribute(
    string targetName, 
    string[]? at) : MethodTargetAttribute(targetName) {
    // Ids of Ats to pair properly
    public string[] At { get; set; } = at ?? [""];
    // TODO: Slice
}