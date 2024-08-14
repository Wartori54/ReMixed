using System;
using System.Collections.Generic;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Collections.Generic;

namespace ReMixed;

public class PatchableMethodDefinition : IMemberDefinition {
    public MetadataToken MetadataToken {
        get => throw new NotSupportedException();
        set => throw new NotSupportedException();
    }

    public Collection<CustomAttribute> CustomAttributes => patchingMethod.CustomAttributes;
    public bool HasCustomAttributes => patchingMethod.HasCustomAttributes;
    public string Name {
        get => patchingMethod.Name;
        set => throw new NotSupportedException();
    }
    public string FullName => patchingMethod.FullName;
    public bool IsSpecialName {
        get => patchingMethod.IsSpecialName;
        set => throw new NotSupportedException();
    }
    public bool IsRuntimeSpecialName { 
        get => patchingMethod.IsRuntimeSpecialName; 
        set => throw new NotSupportedException(); 
    }

    public TypeDefinition DeclaringType {
        get => patchingMethod.DeclaringType;
        set => throw new NotSupportedException();
    }

    private readonly MethodDefinition patchingMethod;
    public ReadOnlyCollection<Instruction> Instructions { get; }

    private readonly Dictionary<int, List<Blob>> injections = new();


    public static PatchableMethodDefinition FromMethodDef(MethodDefinition methodDefinition) {
        return PatchPlatform.Instance!.PatchProvider(methodDefinition);
    }
    
    protected PatchableMethodDefinition(MethodDefinition methodDefinition) {
        patchingMethod = methodDefinition;
        Instructions = new ReadOnlyCollection<Instruction>(methodDefinition.Body.Instructions);
    }

    public MethodPatchContext.Positioner AcquirePositioner() {
        return new MethodPatchContext.Positioner(Instructions);
    }

    public MethodPatchContext.Cursor AcquireCursorFromBlob(MethodPatchContext.Positioner positioner) {
        if (!injections.TryGetValue(positioner.Index, out List<Blob>? blobs)) {
            blobs = [];
            injections[positioner.Index] = blobs;
        }
        Blob blob = new();
        blobs.Add(blob);
        MethodPatchContext.Cursor cursor = new(blob.Instructions, GetBodyProvider());
        return cursor;
    }
    
    public MethodPatchContext.Cursor AcquireCursorFromBlob(MethodPatchContext.Positioner positioner, int size) {
        if (!injections.TryGetValue(positioner.Index, out List<Blob>? blobs)) {
            blobs = [];
            injections[positioner.Index] = blobs;
        }
        ReplaceBlob blob = new(positioner.Index, positioner.Index + size);
        blobs.Add(blob);
        MethodPatchContext.Cursor cursor = new(blob.Instructions, GetBodyProvider());
        return cursor;
    }

    protected virtual CollectionILProcessor.IBodyDataProvider GetBodyProvider() {
        return new BodyProvider(patchingMethod);
    }
    

    public class Blob {
        public readonly Collection<Instruction> Instructions;
        public Blob() : this([]) {
        }

        public Blob(Collection<Instruction> instructions) {
            Instructions = instructions;
        }
    }

    public class ReplaceBlob : Blob {
        public readonly int RepStart;
        public readonly int RepEnd;

        public ReplaceBlob(int repStart, int repEnd, Collection<Instruction>? instructions = null) : base(instructions ?? []) {
            RepStart = repStart;
            RepEnd = repEnd;
        }
    }
    
    private class BodyProvider(MethodDefinition methodDefinition) : CollectionILProcessor.IBodyDataProvider {
        public Collection<VariableDefinition> Variables => methodDefinition.Body.Variables;
        public ParameterDefinition? GetParameter(int index) {
            if (methodDefinition.HasThis) {
                if (index == 0)
                    return methodDefinition.Body.ThisParameter;

                index--;
            }

            Collection<ParameterDefinition>? parameters = methodDefinition.Parameters;

            if (index < 0 || index >= parameters.Count)
                return null;

            return parameters [index];
        }
        
        public TypeReference GetReturnType() {
            return methodDefinition.ReturnType;
        }
        public void AddVariable(VariableDefinition variableDefinition) {
            methodDefinition.Body.Variables.Add(variableDefinition);
        }
        
        public bool HasThis => methodDefinition.HasThis;
    }
}
