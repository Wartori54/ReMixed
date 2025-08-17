using System;
using System.Collections.Generic;
using System.Reflection;
using Mono.Cecil;
using MonoMod.Cil;
using MonoMod.RuntimeDetour;

namespace ReMixed.PlatformImpls;

public class MonoModPlatform : PatchPlatform {
    private const string PlatformName = "MonoMod";

    protected readonly List<ILHook> hooks = [];
    protected readonly Dictionary<MethodDefinition, MonoModPatchContext> DMDToContext = new();

    public override PatchableMethodDefinition.IMethodPool PatchableMethodPool { get; }


    public MonoModPlatform(ThisCecilDefs.IThisCecilDefsProvider thisCecilDefsProvider) : base(PlatformName, thisCecilDefsProvider) {
        PatchableMethodPool = new MonoModMethodPool(this);
    }

    // public override void ApplyPatch(MethodBase target, Action<MethodPatchContext.LegCursor> patch) {
    //     hooks.Add(new ILHook(target, GetManipulator(patch, target)));
    // }

    public override MethodPatchContext MethodPatchContextFor(MethodDefinition method) {
        throw new NotImplementedException();
    }

    public override void Flush() {
    }
    public override void Dispose() {
        foreach (ILHook ilHook in hooks) {
            ilHook.Dispose();
        }
        DMDToContext.Clear();
        base.Dispose();
    }
    
    private ILContext.Manipulator GetManipulator(Action<MethodPatchContext.LegCursor> action, MethodBase method) => ctx => {
        MonoModPatchContext mmCtx = new(ctx, method, this);
        DMDToContext[mmCtx.Method] = mmCtx;
        MethodPatchContext.LegCursor cursor = mmCtx.GetCursor();
        action(cursor);
    };
    
    private class MonoModMethodPool(MonoModPlatform platform) : PatchableMethodDefinition.IMethodPool {
        public PatchableMethodDefinition Obtain(MethodDefinition methodDefinition) {
            throw new NotImplementedException();
            return new MonoModPatchableMethodDefinition(methodDefinition, platform.DMDToContext[methodDefinition].GetRealMethod());
        }
    }
}