using System;
using System.Collections.Generic;
using System.Linq;
using Mono.Cecil;
using Mono.Collections.Generic;
using ReMixed.Processor;
using ReMixed.Transformer;

namespace ReMixed;

public static class MixinApply {
    public static void MixinMergeAndRelink(PatchPlatform platform, ModuleDefinition module) {
        Dictionary<FieldReference, FieldReference> fields = new();
        Dictionary<PropertyReference, PropertyDefinition> properties = new();
        Dictionary<EventReference, EventDefinition> events = new();
        Dictionary<MethodReference, MethodDefinition> methods = new();
        Dictionary<TypeReference, TypeDefinition> mergedTypes = new();
        List<TypeDefinition> targetTypes = new();
        // Merge all types
        MixinMergerTransformer currTr = new(platform, fields, properties, events, methods);
        foreach (TypeDefinition type in module.Types) { // TODO: Nested
            if (!currTr.AppliesTo(type)) continue;
            TypeDefinition? typeTarget = module.Types.BetterFirst(currTr.GetTargetPredicate(type));
            if (typeTarget == null) {
                throw new InvalidOperationException($"{nameof(MixinMergerTransformer)} matched no target type with source type: {type}");
            }
            currTr.Perform(type, typeTarget);
            targetTypes.Add(typeTarget);
            mergedTypes.Add(type, typeTarget);
                
            currTr = new MixinMergerTransformer(platform, fields, properties, events, methods);
        }
        
        // Relink!
        foreach (TypeDefinition targetType in targetTypes) {
            RelinkerProcessor relinkerProcessor = new(
                RelinkMapType,
                RelinkMapField,
                RelinkMapMethod
            );
            if (!relinkerProcessor.Applies(targetType)) {
                throw new InvalidOperationException();
            }
            relinkerProcessor.Process(targetType);

            TypeReference? RelinkMapType(TypeReference type) {
                // If it was merged, redirect to destination
                return mergedTypes.GetValueOrDefault(type);
            }

            FieldReference? RelinkMapField(FieldReference field) {
                return fields.GetValueOrDefault(field);
            }

            MethodReference? RelinkMapMethod(MethodReference method) {
                return methods.GetValueOrDefault(method);
            }
        }
        // Done!
    }

    public static T? BetterFirst<T>(this IEnumerable<T> source, Predicate<T> pred) where T : class {
        foreach (T e in source) {
            if (pred(e)) return e;
        }
        return null;
    }
}