using Mono.Cecil.Cil;
using ReMixed.Injection;

namespace ReMixed.Positioning;

public static class Positioners {
    public static void Register(Injector.InjectorRegistry registry) {
        registry.RegisterPositioner("HEAD", HeadAction);
        registry.RegisterPositioner("TAIL", TailAction);
    }

    // Absolute positioners
    private static bool HeadAction(MethodPatchContext.Positioner p) {
        p.GotoFirst();
        return false;
    }

    private static bool TailAction(MethodPatchContext.Positioner p) {
        p.GotoLast();
        if (p.Previous?.OpCode == OpCodes.Ret) {
            p.MoveIndex(-1);
        }
        return false;
    }

    private static bool BeforeRetAction(MethodPatchContext.Positioner p) {
        return p.TryGotoNext(i => i.OpCode == OpCodes.Ret);
    }
}