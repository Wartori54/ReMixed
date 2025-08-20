using System;
using System.Collections.Generic;
using Mono.Cecil;
using Mono.Collections.Generic;
using ReMixed.PlatformImpls;
using ReMixed.Processor;
using ReMixed.Transformer;

namespace ReMixed;

public static class MixinApply {
    public static void MixinMergeAndRelink(PatchPlatform platform, ModuleDefinition module) {
        const string id = "NOID";
        RelinkerConfig rconfig = new();
        List<TypeDefinition> targetTypes = [];
        List<ITransformerFactory<FieldDefinition, FieldDefinition>> fieldTransformerFactories = [
            new FieldRetargetMixin.Factory(rconfig)
        ];
        List<ITransformerFactory<MethodDefinition, MethodDefinition>> methodTransformerFactories = [
            new MethodMPATransformer.Factory(platform)
        ];
        // Merge all types
        MixinMergerTransformer.Factory trFactory = new(platform, id, rconfig);
        TypeMoverGenerator.Factory tMoverFactory = new(platform, id, rconfig);
        foreach (TypeDefinition type in module.Types) {
            IEnumerable<TypeDefinition>? targets = trFactory.AppliesTo(type, module.Types);
            if (targets == null) {
                continue;
            }
            
            foreach (TypeDefinition typeTarget in targets) {
                foreach (ITransformerFactory<FieldDefinition, FieldDefinition> trFact in fieldTransformerFactories) {
                    ApplySymmetricTransformer(trFact, type.Fields, typeTarget.Fields);
                }
                trFactory.For(type, typeTarget).Perform(type, typeTarget);
                targetTypes.Add(typeTarget);
                rconfig.Moved(type, typeTarget);
                int origCount = type.NestedTypes.Count;
                for (int i = 0; i < origCount; i++) {
                    TypeDefinition nested = type.NestedTypes[i];
                    if (!tMoverFactory.Applies(nested)) {
                        continue;
                    }
                    tMoverFactory.For(nested, typeTarget.NestedTypes).Process(nested, typeTarget.NestedTypes);
                }
                foreach (ITransformerFactory<MethodDefinition, MethodDefinition> trFact in methodTransformerFactories) {
                    ApplySymmetricTransformer(trFact, typeTarget.Methods, typeTarget.Methods);
                }
            }
        }
        platform.Flush();

        // Relink!
        foreach (TypeDefinition targetType in targetTypes) {
            RelinkerProcessor relinkerProcessor = new(
                rconfig
            );
            if (!relinkerProcessor.Applies(targetType)) {
                throw new InvalidOperationException();
            }
            relinkerProcessor.Process(targetType);

            foreach (TypeDefinition nested in targetType.NestedTypes) {
                relinkerProcessor.Process(nested);
            }
        }
        // Done!
    }

    public static void ApplySymmetricTransformer<T>(ITransformerFactory<T, T> factory, Collection<T> patches, Collection<T> targets) where T : IMemberDefinition {
        foreach (T patch in patches) {
            IEnumerable<T>? predTargets = factory.AppliesTo(patch, targets);
            if (predTargets == null) continue;
            foreach (T predTarget in predTargets) {
                factory.For(patch, predTarget).Perform(patch, predTarget);
            }
        }
    }

    public static T? BetterFirst<T>(this IEnumerable<T> source, Predicate<T> pred) where T : class {
        foreach (T e in source) {
            if (pred(e)) return e;
        }
        return null;
    }
}