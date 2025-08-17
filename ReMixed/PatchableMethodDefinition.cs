using System;
using System.Collections.Generic;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Collections.Generic;

namespace ReMixed;

public class PatchableMethodDefinition : IMemberDefinition, IMethodSignature, IGenericParameterProvider {
    public MetadataToken MetadataToken {
        get => throw new NotSupportedException();
        set => throw new NotSupportedException();
    }

    private readonly ReadOnlyCollection<CustomAttribute> customAttributes;

    public Collection<CustomAttribute> CustomAttributes => customAttributes;
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

    public bool HasThis {
        get => patchingMethod.HasThis;
        set => throw new NotSupportedException();
    }
    
    public bool ExplicitThis {
        get => patchingMethod.ExplicitThis; 
        set => throw new NotSupportedException();
    }

    public MethodCallingConvention CallingConvention {
        get => patchingMethod.CallingConvention;
        set => throw new NotSupportedException();
    }
    
    public bool HasParameters => patchingMethod.HasParameters;

    private readonly ReadOnlyCollection<ParameterDefinition> parameters;
    public Collection<ParameterDefinition> Parameters => parameters;

    public TypeReference ReturnType {
        get => patchingMethod.ReturnType;
        set => throw new NotSupportedException();
    }
    
    public MethodReturnType MethodReturnType => patchingMethod.MethodReturnType;
    

    public bool HasGenericParameters => patchingMethod.HasGenericParameters;
    public bool IsDefinition => patchingMethod.IsDefinition;
    public ModuleDefinition Module => throw new NotSupportedException();
    private readonly ReadOnlyCollection<GenericParameter> genericParameters;
    public Collection<GenericParameter> GenericParameters => genericParameters;
    public GenericParameterType GenericParameterType => GenericParameterType.Method; // We are patching methods
    
    private readonly MethodDefinition patchingMethod;
    
    // public MethodReference Reference => patchingMethod;
    public ReadOnlyCollection<Instruction> Instructions { get; }
    
    private readonly List<Blob> blobList = [];
    private bool isApplied;

    public static PatchableMethodDefinition FromMethodDef(MethodDefinition methodDefinition, MethodPatchContext context) {
        return context.Platform.PatchableMethodPool.Obtain(methodDefinition);
    }
    
    public PatchableMethodDefinition(MethodDefinition methodDefinition) {
        patchingMethod = methodDefinition;
        Instructions = new ReadOnlyCollection<Instruction>(methodDefinition.Body.Instructions);
        customAttributes = new ReadOnlyCollection<CustomAttribute>(methodDefinition.CustomAttributes);
        parameters = new ReadOnlyCollection<ParameterDefinition>(methodDefinition.Parameters);
        genericParameters = new ReadOnlyCollection<GenericParameter>(methodDefinition.GenericParameters);
    }

    public MethodPatchContext.Positioner AcquirePositioner() {
        return new MethodPatchContext.Positioner(Instructions);
    }

    public MethodPatchContext.Cursor AcquireCursorFromBlob(MethodPatchContext.Positioner positioner, int size) {
        CheckState();
        Blob blob = NewBlob(positioner.Index, size);
        MethodPatchContext.Cursor cursor = new(blob.BlobInstructions, GetBodyProvider());
        return cursor;
    }

    public MethodPatchContext.Cursor AcquireCursorFromBlob(MethodPatchContext.Positioner positioner, MethodPatchContext.Positioner endPositioner) {
        return AcquireCursorFromBlob(positioner, endPositioner.Index-positioner.Index);
    }
    
    public void Apply() {
        if (isApplied) return;
        isApplied = true;
        
        Collection<Instruction> targetInstrs = patchingMethod.Body.Instructions;

        for (int blobIdx = blobList.Count - 1; blobIdx >= 0; blobIdx--) {
            Blob blob = blobList[blobIdx];
            for (int i = 0; i < blob.GlobalSize && i < blob.BlobInstructions.Count; i++) { // Replace as many instrs as possible
                targetInstrs[i + blob.GlobalIndex] = blob.BlobInstructions[i];
            }

            if (blob.GlobalSize < blob.BlobInstructions.Count) { // Insert the leftover ones
                for (int i = blob.GlobalSize; i < blob.BlobInstructions.Count; i++) {
                    targetInstrs.Insert(blob.GlobalIndex + blob.GlobalSize, blob.BlobInstructions[i]);
                }
            } else if (blob.GlobalSize > blob.BlobInstructions.Count) { // Remove the leftover ones
                for (int i = blob.BlobInstructions.Count; i < blob.GlobalSize; i++) {
                    targetInstrs.RemoveAt(blob.GlobalIndex + i);
                }
            }
        }
        blobList.Clear();
        
        // Done!
    }

    protected virtual CollectionILProcessor.IBodyDataProvider GetBodyProvider() {
        return new BodyProvider(patchingMethod);
    }

    // Get a new blob in the bounds specified and by cloning the instructions
    private Blob NewBlob(int index, int size) {
        Blob blob = new(InstructionSubClone(index, size), index, size);
        if (blobList.Count == 0) {
            blobList.Add(blob);
            return blob;
        }
        for (int i = 0; i < blobList.Count; i++) {
            if (blobList[i].GlobalIndex > index) continue;
            if (blobList[i].GlobalIndex + blobList[i].GlobalSize > index) 
                throw new InvalidOperationException("Overlapping blobs!");
            // TODO: Better error reporting through metadata
            blobList.Insert(i+1, blob); // Insert it right after
            break;
        }
        return blob;
    }

    private Collection<Instruction> InstructionSubClone(int index, int size) {
        Collection<Instruction> instructions = new(size);
        for (int i = index; i < index + size; i++) {
            // you need to hack cecil this way to clone instructions, the ctor is not public
            Instruction curr = Instructions[i].Clone();
            instructions.Add(curr);
        }
        return instructions;
    }

    private void CheckState() {
        if (isApplied) throw new InvalidOperationException("Patchable method has been already applied!");
    }

    public struct Blob {
        public readonly Collection<Instruction> BlobInstructions;
        public readonly int GlobalIndex;
        public readonly int GlobalSize;
        // public readonly MetaData meta; // TODO
        
        public Blob(Collection<Instruction> instructions, int globalIndex, int globalSize) {
            BlobInstructions = instructions;
            GlobalIndex = globalIndex;
            GlobalSize = globalSize;
        }
    }

    private sealed class BodyProvider(MethodDefinition methodDefinition) : CollectionILProcessor.IBodyDataProvider {
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

    public interface IMethodPool {
        public PatchableMethodDefinition Obtain(MethodDefinition methodDefinition);
    }

}
