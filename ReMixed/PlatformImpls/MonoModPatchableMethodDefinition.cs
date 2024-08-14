using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Collections.Generic;

namespace ReMixed.PlatformImpls;

public class MonoModPatchableMethodDefinition : PatchableMethodDefinition {
    private readonly MethodDefinition origMethod;
    private readonly MethodDefinition patchingMethod;
    public MonoModPatchableMethodDefinition(MethodDefinition writeMethod, MethodDefinition readMethod) : base(writeMethod) {
        origMethod = readMethod;
        patchingMethod = writeMethod;
    }

    protected override CollectionILProcessor.IBodyDataProvider GetBodyProvider() {
        return new MethodBodyDMDCombiner(origMethod.Body, patchingMethod.Body);
    }
    
    public class MethodBodyDMDCombiner(MethodBody readBody, MethodBody writeBody) : CollectionILProcessor.IBodyDataProvider {
        public Collection<VariableDefinition> Variables => writeBody.Variables;
        public ParameterDefinition? GetParameter(int index) {
            // TODO: Cleanup this
            if (!readBody.Method.IsStatic && writeBody.Method.IsStatic) {
                // Ugly edge case for monomod's dmds
                if (index < 0 || index >= writeBody.Method.Parameters.Count)
                    return null;
                // The instance parameter has moved to the first parameter of the dmd, as such just blindly pair it
                return writeBody.Method.Parameters[index];
            }
            
            MethodDefinition? method = readBody.Method;

            if (method.HasThis) {
                if (index == 0)
                    return readBody.ThisParameter;

                index--;
            }

            Collection<ParameterDefinition>? parameters = method.Parameters;

            if (index < 0 || index >= parameters.Count)
                return null;

            return parameters [index];
        }

        public TypeReference GetReturnType() => readBody.Method.ReturnType;

        public void AddVariable(VariableDefinition variableDefinition) {
            writeBody.Variables.Add(variableDefinition);
        }

        public bool HasThis => readBody.Method.HasThis;
    }
}