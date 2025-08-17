namespace ReMixed.MethodAttribute;

public class OverwriteAttribute(string methodTarget) : MethodPositionedAttribute(methodTarget, ["HEAD"]) {
    
}