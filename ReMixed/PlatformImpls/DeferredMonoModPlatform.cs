using System;
using System.Collections.Generic;
using System.Reflection;
using MonoMod.Cil;
using MonoMod.RuntimeDetour;

namespace ReMixed.PlatformImpls;

public class DeferredMonoModPlatform : MonoModPlatform {
    private readonly ActionPatchCollection patches = new();
    
    public DeferredMonoModPlatform(ThisCecilDefs.IThisCecilDefsProvider provider) : base(provider) {
    }

    public override void ApplyPatch(MethodBase target, Action<IPatchContext.Cursor> patch) {
        patches.AddPatch(target, patch);
    }
    
    public void ApplyAll() {
        patches.AllMethods(mb => {
            hooks.Add(new ILHook(mb, ctx => {
                MonoModContext mmCtx = new(ctx, mb);
                patches.RunPatchesFor(mb, mmCtx.ContextCursor);
            }));
        });
    }


    // public record DeferredILHook(MethodBase target, Action<PatchContext.Cursor> action) {
    //     private ILHook? inner;
    //     public bool IsApplied => inner != null;
    //
    //     public void Apply() {
    //         if (inner != null) throw new InvalidOperationException("This hook as applied twice!");
    //         inner = new ILHook(target, GetManipulator(action, target));
    //     }
    //
    //     public void Undo() {
    //         if (inner == null) return;
    //         inner.Undo();
    //         inner.Dispose();
    //         inner = null;
    //     }
    //
    // }
}

public class ActionPatchCollection {
    private readonly Dictionary<MethodBase, List<Action<IPatchContext.Cursor>>> patches = [];

    // Patch ordering is not supported yet
    public void AddPatch(MethodBase target, Action<IPatchContext.Cursor> patch) {
        if (!patches.TryGetValue(target, out List<Action<IPatchContext.Cursor>>? patchList)) {
            patchList = new();
            patches[target] = patchList;
        }
        patchList.Add(patch);
    }

    public void AllMethods(Action<MethodBase> run) {
        foreach ((MethodBase mb, _) in patches) {
            run(mb);
        }
    }

    public void RunPatchesFor(MethodBase target, IPatchContext.Cursor cursor) {
        foreach (Action<IPatchContext.Cursor> patch in patches[target]) {
            patch(cursor);
        }
    }
}
