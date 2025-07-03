namespace ReMixed.PlatformImpls;

public class CecilPlatform : PatchPlatform {

    public CecilPlatform() : base("CecilPlatform", new DefaultThisCecilDefsProvider()) {
        
    }
    
    public override PatchableMethodDefinition.IMethodPool PatchableMethodPool { get; } = null!;

}
