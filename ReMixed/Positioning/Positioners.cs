using Mono.Cecil;
using Mono.Cecil.Cil;
using ReMixed.Injection;
using ReMixed.MethodAttribute;

namespace ReMixed.Positioning;

public static class Positioners {
    public static void Register(Injector.InjectorRegistry registry) {
        registry.RegisterPositioner("HEAD", HeadAction);
        registry.RegisterPositioner("TAIL", TailAction);
        registry.RegisterPositioner("RETURN", BeforeRetAction);
        registry.RegisterPositioner("CALL", AtCallAction);
    }

    // Absolute positioners
    private static bool HeadAction(MethodPatchContext.Positioner p, AtPosAttribute? _, int itr) {
        p.GotoFirst();
        return itr == 0;
    }
    
    private static bool TailAction(MethodPatchContext.Positioner p, AtPosAttribute? _, int itr) {
        // Going to the last instruction is not enough, since for:
        // if (a) {
        //     return x;
        // }
        // return y;
        // On Release it does what you'd expect, but on Debug it generates the following
        // l1: brfalse l4
        // l2: ld x
        // l3: br l5
        // l4: ld y
        // l5: ret
        // Which changes the behaviour of this positioner. Decompilers are able to see through this
        // and decompile to the same code in either case.
        // TODO: Add special handling for br to ret in Debug mode
        p.GotoLast();
        if (p.Previous?.OpCode == OpCodes.Ret) {
            p.MoveIndex(-1);
        }
        return itr == 0;
    }

    private static bool BeforeRetAction(MethodPatchContext.Positioner p, AtPosAttribute? _, int itr) {
        if (itr == 0) return p.TryGotoNext(i => i.OpCode == OpCodes.Ret);
        p.MoveIndex(1);
        return p.TryGotoNext(i => i.OpCode == OpCodes.Ret);
    }

    private static bool AtCallAction(MethodPatchContext.Positioner p, AtPosAttribute? atPosAttribute, int itr) {
        if (itr != 0) return false;
        if (atPosAttribute == null) return false;
        return p.TryGotoNext(i => (i.OpCode == OpCodes.Call || i.OpCode == OpCodes.Callvirt) &&
                                  ((MethodReference)i.Operand).FullName == atPosAttribute.TargetEl);
    }
}