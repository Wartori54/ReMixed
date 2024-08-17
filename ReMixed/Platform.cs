using System;
using System.Reflection;
using Mono.Cecil;

namespace ReMixed;

public abstract class PatchPlatform : IDisposable {

    // TODO: Allow multiple simultaneous platforms?
    public static PatchPlatform? Instance { get; private set; }
    public string Name { get; }

    public ThisCecilDefs ThisCecilDefs { get; }

    public ILPatcher ILPatcher { get; }
    
    public abstract Func<MethodDefinition, PatchableMethodDefinition> PatchProvider { get; }

    protected PatchPlatform(string name, ThisCecilDefs.IThisCecilDefsProvider thisCecilDefsProvider) {
        ThisCecilDefs = thisCecilDefsProvider.Get();
        Name = name;
        ILPatcher = new ILPatcher(ThisCecilDefs);
        if (Instance != null) throw new InvalidOperationException();
        Instance = this;
    }

    public abstract void ApplyPatch(MethodBase target, Action<MethodPatchContext.LegCursor> patch);


    
    public virtual void Dispose() {
        Instance = null;
    }
}