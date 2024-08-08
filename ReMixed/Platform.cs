using System;
using System.Reflection;

namespace ReMixed;

public abstract class PatchPlatform {
    public string Name { get;}

    public ThisCecilDefs.IThisCecilDefsProvider ThisCecilDefsProvider;

    public ILPatcher ILPatcher { get; }

    protected PatchPlatform(string name, ThisCecilDefs.IThisCecilDefsProvider thisCecilDefsProvider) {
        ThisCecilDefsProvider = thisCecilDefsProvider;
        Name = name;
        ILPatcher = new ILPatcher(thisCecilDefsProvider.Get());
    }

    public abstract void ApplyPatch(MethodBase target, Action<IPatchContext.Cursor> patch);
}