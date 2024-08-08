using System;
using System.Collections.Generic;
using System.Reflection;
using MonoMod.Cil;
using MonoMod.RuntimeDetour;

namespace ReMixed.PlatformImpls;

public class MonoModPlatform : PatchPlatform, IDisposable {
    private const string PlatformName = "MonoMod";

    protected List<ILHook> hooks = [];

    public MonoModPlatform(ThisCecilDefs.IThisCecilDefsProvider thisCecilDefsProvider) : base(PlatformName, thisCecilDefsProvider) {
    }

    public override void ApplyPatch(MethodBase target, Action<IPatchContext.Cursor> patch) {
        hooks.Add(new ILHook(target, GetManipulator(patch, target)));
    }

    public void Dispose() {
        foreach (ILHook ilHook in hooks) {
            ilHook.Dispose();
        }
    }
    
    private static ILContext.Manipulator GetManipulator(Action<IPatchContext.Cursor> action, MethodBase method) => ctx => {
        MonoModContext mmCtx = new(ctx, method);
        MonoModContext.MonoModCursor cursor = new(mmCtx);
        action(cursor);
    };
}