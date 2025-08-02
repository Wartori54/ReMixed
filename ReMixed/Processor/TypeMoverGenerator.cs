using System.Collections.Generic;
using System.Linq;
using Mono.Cecil;
using Mono.Collections.Generic;
using ReMixed.Transformer;

namespace ReMixed.Processor;

public class TypeMoverGenerator : IGenerator<TypeDefinition, Collection<TypeDefinition>> {
    private readonly PatchPlatform platform;
    
    private readonly Dictionary<FieldRefUID, FieldReference> copiedFields;
    private readonly Dictionary<PropertyRefUID, PropertyDefinition> copiedProperties;
    private readonly Dictionary<EventRefUID, EventDefinition> copiedEvents;
    private readonly Dictionary<MethodRefUID, MethodDefinition> copiedMethods;

    public TypeMoverGenerator(PatchPlatform patchPlatform, 
        Dictionary<FieldRefUID, FieldReference>? fields = null,
        Dictionary<PropertyRefUID, PropertyDefinition>? properties = null,
        Dictionary<EventRefUID, EventDefinition>? events = null,
        Dictionary<MethodRefUID, MethodDefinition>? methods = null) {
        platform = patchPlatform;
        copiedFields = fields ?? new();
        copiedProperties = properties ?? new();
        copiedEvents = events ?? new();
        copiedMethods = methods ?? new();
    }

    public bool Applies(TypeDefinition target) {
        foreach (CustomAttribute customAttribute in target.CustomAttributes) {
            if (ILPatcher.TypeReferenceEqual(customAttribute.AttributeType, platform.ThisCecilDefs.MixinAttribute)) return false;
        }
        return true;
    }

    public void Process(TypeDefinition target, Collection<TypeDefinition> container) {
        bool dup = container.Any(t => t.Name == target.Name);
        TypeDefinition newType = target.CloneTDefIdentity();
        if (dup) {
            newType.Name = "Dup_" + newType.Name;
        }
        container.Add(newType);
        MixinMergerTransformer mmTransformer = new(platform, copiedFields, copiedProperties, copiedEvents, copiedMethods);
        mmTransformer.Perform(target, newType);
    }
}