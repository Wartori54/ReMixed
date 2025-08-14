using System.Collections.Generic;
using System.Linq;
using Mono.Cecil;
using Mono.Collections.Generic;
using ReMixed.Transformer;

namespace ReMixed.Processor;

public class TypeMoverGenerator : IGenerator<TypeDefinition, Collection<TypeDefinition>> {
    private readonly RelinkerConfig relinkerConfig;
    private readonly string prefixId;

    private TypeMoverGenerator(
        string id,
        RelinkerConfig rconfig
        ) {
        prefixId = id;
        relinkerConfig = rconfig;
    }

    public sealed class Factory : IGeneratorFactory<TypeDefinition, Collection<TypeDefinition>> {
        private readonly PatchPlatform platform;
    
        private readonly RelinkerConfig relinkerConfig;
        private readonly string prefixId;

        public Factory(PatchPlatform patchPlatform,
            string id,
            RelinkerConfig rconfig
        ) {
            platform = patchPlatform;
            prefixId = id;
            relinkerConfig = rconfig;
        }

        public bool Applies(TypeDefinition target) {
            foreach (CustomAttribute customAttribute in target.CustomAttributes) {
                if (ILPatcher.TypeReferenceEqual(customAttribute.AttributeType, platform.ThisCecilDefs.MixinAttribute)) return false;
            }
            return true;
        }
        public IGenerator<TypeDefinition, Collection<TypeDefinition>> For(TypeDefinition target, Collection<TypeDefinition> dest) {
            return new TypeMoverGenerator(prefixId, relinkerConfig);
        }
    }

    public void Process(TypeDefinition target, Collection<TypeDefinition> container) {
        bool dup = container.Any(t => t.Name == target.Name);
        TypeDefinition newType = target.CloneTDefIdentity();
        if (dup) {
            newType.Name = "Dup_" + newType.Name;
        }
        container.Add(newType);
        MixinMergerTransformer mmTransformer = new(prefixId, relinkerConfig);
        mmTransformer.Perform(target, newType);
    }
}