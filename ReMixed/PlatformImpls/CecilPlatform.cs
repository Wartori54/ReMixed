using System;
using System.Collections.Generic;
using System.Reflection;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Collections.Generic;
using MethodBody = Mono.Cecil.Cil.MethodBody;

namespace ReMixed.PlatformImpls;

public class CecilPlatform : PatchPlatform {

    public CecilPlatform() : base("CecilPlatform", new DefaultThisCecilDefsProvider()) {
        
    }
    
    public override PatchableMethodDefinition.IMethodPool PatchableMethodPool { get; } = new MethodPool();
    public override MethodPatchContext MethodPatchContextFor(MethodDefinition method) {
        return new CecilMethodPatchContext(method, this);
    }


    public override void Flush() {
        (PatchableMethodPool as MethodPool)!.Flush();
    }
    
    private sealed class MethodPool : PatchableMethodDefinition.IMethodPool {
        private readonly Dictionary<MethodRefUID, PatchableMethodDefinition> methods = new();
        public PatchableMethodDefinition Obtain(MethodDefinition methodDefinition) {
            MethodRefUID mRefUID = methodDefinition.ToUID();
            if (methods.TryGetValue(mRefUID, out PatchableMethodDefinition? value)) return value;
            return methods[mRefUID] = new PatchableMethodDefinition(methodDefinition);
        }

        public void Flush() {
            foreach (PatchableMethodDefinition method in methods.Values) {
                method.Apply();
            }
        }
    }

    private sealed class CecilMethodPatchContext : MethodPatchContext {

        public CecilMethodPatchContext(MethodDefinition patchingMethod, PatchPlatform platform) : base(patchingMethod, platform) {
        }
        protected override Func<object, MethodBody, Collection<Instruction>, object>? GetStAnalysisConverter() {
            return null;
        }
        
        public override MethodDefinition GetRealMethod() {
            return Method;
        }
        public override MethodReference ImportMethod(MethodBase method) {
            return Platform.ThisCecilDefs.ThisModule.ImportReference(method);
        }
    }

}
