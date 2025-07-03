using System;
using System.Collections.Generic;
using System.Reflection;
using MonoMod.RuntimeDetour;

namespace ReMixed.PlatformImpls;

public class DeferredMonoModPlatform : MonoModPlatform {
    private readonly ActionPatchCollection patches = new();
    
    public DeferredMonoModPlatform(ThisCecilDefs.IThisCecilDefsProvider provider) : base(provider) {
    }

    public void ApplyPatch(MethodBase target, Action<MethodPatchContext.LegCursor> patch) {
        patches.AddPatch(target, patch);
    }
    
    public void ApplyAll() {
        patches.AllMethods(mb => {
            hooks.Add(new ILHook(mb, ctx => {
                MonoModPatchContext mmCtx = new(ctx, mb, this);
                patches.RunPatchesFor(mb, mmCtx.GetCursor());
                // MethodPatchContext.LogAllInstrs(mmCtx);
                // try {
                    // foreach (StackAnalysis.StackFrame stackFrame in new StackAnalysis(mmCtx.Method, MonoModContext.StAnalysisConvert).StackFrames) {
                        // Console.WriteLine(stackFrame.Elements);
                    // }
                    // Console.WriteLine(mmCtx.Method.Body.Instructions.Count);
                    // foreach (VariableDefinition variable in mmCtx.Method.Body.Variables) {
                        // Console.WriteLine($"{variable.Index} {variable.VariableType.FullName}");
                    // }
                // } catch (NotImplementedException) {
                // }
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
    private readonly Dictionary<MethodBase, List<Action<MethodPatchContext.LegCursor>>> patches = [];

    // Patch ordering is not supported yet
    public void AddPatch(MethodBase target, Action<MethodPatchContext.LegCursor> patch) {
        if (!patches.TryGetValue(target, out List<Action<MethodPatchContext.LegCursor>>? patchList)) {
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

    public void RunPatchesFor(MethodBase target, MethodPatchContext.LegCursor cursor) {
        foreach (Action<MethodPatchContext.LegCursor> patch in patches[target]) {
            patch(cursor);
        }
    }
}
