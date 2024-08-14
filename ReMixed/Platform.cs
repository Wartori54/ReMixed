using System;
using System.Reflection;
using Mono.Cecil;

namespace ReMixed;

public abstract class PatchPlatform : IDisposable {

    public static PatchPlatform? Instance { get; private set; }
    public string Name { get;}

    public ThisCecilDefs.IThisCecilDefsProvider ThisCecilDefsProvider;

    public ILPatcher ILPatcher { get; }
    
    public abstract Func<MethodDefinition, PatchableMethodDefinition> PatchProvider { get; }

    protected PatchPlatform(string name, ThisCecilDefs.IThisCecilDefsProvider thisCecilDefsProvider) {
        ThisCecilDefsProvider = thisCecilDefsProvider;
        Name = name;
        ILPatcher = new ILPatcher(thisCecilDefsProvider.Get());
        if (Instance != null) throw new InvalidOperationException();
        Instance = this;
    }

    public abstract void ApplyPatch(MethodBase target, Action<MethodPatchContext.LegCursor> patch);


    
    public virtual void Dispose() {
        Instance = null;
    }
}