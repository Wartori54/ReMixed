using ReMixed.Registry;

namespace ReMixed.Positioning;

[AtId("HEAD")]
public class HeadPositioner : Positioner {
    public override void Do(MethodPatchContext.Positioner positioner) {
        positioner.GotoFirst();
    }
}