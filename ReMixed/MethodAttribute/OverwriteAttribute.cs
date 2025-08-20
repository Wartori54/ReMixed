namespace ReMixed.MethodAttribute;

public class OverwriteAttribute : MethodPositionedAttribute {
    public OverwriteAttribute() : base(null, ["HEAD"]) {
    }
    public OverwriteAttribute(string methodTarget) : base(methodTarget, ["HEAD"]) {
    }

}