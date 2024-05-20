using System;
using System.Collections.Generic;
using System.Reflection;
using MonoMod.Cil;
using MonoMod.RuntimeDetour;

namespace ReMixed.PlatformImpls;

public class DeferredMonoModPlatform : MonoModPlatform {
    private readonly ActionPatchCollection<PatchPlatform.Cursor> methodPatches;
    
    public DeferredMonoModPlatform(ILContext ctx, MethodBase origMethod,
        ActionPatchCollection<PatchPlatform.Cursor> patches) : base(ctx, origMethod) {
        methodPatches = patches;
    }
    
    private void Apply() {
        MonoModCursor cursor = new(this);
        methodPatches.RunPatches(cursor);
    }
    
    // Global state
    private static readonly Dictionary<MethodBase, ActionPatchCollection<PatchPlatform.Cursor>> allPatches = new();
    private static readonly Dictionary<MethodBase, ILHook> appliedPatches = new();

    public new static void AutoHook(MethodBase method, Action<PatchPlatform.Cursor> action) {
        if (!allPatches.TryGetValue(method, out ActionPatchCollection<PatchPlatform.Cursor>? patches)) {
            patches = new ActionPatchCollection<PatchPlatform.Cursor>();
            allPatches.Add(method, patches);
        }
        
        patches.AddPatch(action);
    }

    public static void ApplyAll() {
        foreach ((MethodBase target, ActionPatchCollection<PatchPlatform.Cursor> patches) in allPatches) {
            if (appliedPatches.TryGetValue(target, out ILHook? hook)) { // If already patched we will have to undo :(
                Console.WriteLine("Late patching is not recommended!");
                hook.Undo();
                hook.Dispose();
            }
            
            // Otherwise do the patch
            hook = new ILHook(target, ctx => {
                DeferredMonoModPlatform platform = new(ctx, target, patches);
                platform.Apply();
            }, false);
            
            appliedPatches[target] = hook;
            hook.Apply();
        }
    }


    // public record DeferredILHook(MethodBase target, Action<PatchPlatform.Cursor> action) {
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

public class ActionPatchCollection<T> {
    private readonly List<Action<T>> patches = [];

    // Patch ordering is not supported yet
    public void AddPatch(Action<T> patch) => patches.Add(patch);

    public void RunPatches(T cursor) {
        foreach (Action<T> patch in patches) {
            patch(cursor);
        }
    }
}
