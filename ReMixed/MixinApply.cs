using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using Mono.Cecil;
using Mono.Collections.Generic;
using ReMixed.Processor;
using ReMixed.Transformer;

namespace ReMixed;

public static class MixinApply {
    public static void MixinMergeAndRelink(PatchPlatform platform, ModuleDefinition module) {
        Dictionary<FieldRefUID, FieldReference> fields = new();
        Dictionary<PropertyRefUID, PropertyDefinition> properties = new();
        Dictionary<EventRefUID, EventDefinition> events = new();
        Dictionary<MethodRefUID, MethodDefinition> methods = new();
        Dictionary<TypeReference, TypeDefinition> mergedTypes = new();
        List<TypeDefinition> targetTypes = new();
        // Merge all types
        MixinMergerTransformer currTr = new(platform, fields, properties, events, methods);
        foreach (TypeDefinition type in module.Types) { // TODO: Nested
            if (!currTr.AppliesTo(type)) {
                // TypeMoverGenerator tMover = new(platform, fields, properties, events, methods);
                // Debug.Assert(tMover.Applies(type));
                // tMover.Process(type, module.Types);
                continue;
            }
            TypeDefinition? typeTarget = module.Types.BetterFirst(currTr.GetTargetPredicate(type));
            if (typeTarget == null) {
                throw new InvalidOperationException($"{nameof(MixinMergerTransformer)} matched no target type with source type: {type}");
            }
            currTr.Perform(type, typeTarget);
            targetTypes.Add(typeTarget);
            mergedTypes.Add(type, typeTarget);
            TypeMoverGenerator tMover = new(platform, fields, properties, events, methods);
            int origCount = type.NestedTypes.Count;
            for (int i = 0; i < origCount; i++) {
                TypeDefinition nested = type.NestedTypes[i];
                if (!tMover.Applies(nested)) {
                    continue;
                }
                tMover.Process(nested, typeTarget.NestedTypes);

                tMover = new TypeMoverGenerator(platform, fields, properties, events, methods);
            }

            currTr = new MixinMergerTransformer(platform, fields, properties, events, methods);
        }

        foreach (KeyValuePair<FieldRefUID, FieldReference> kvp in fields) {
            if (kvp.Value.DeclaringType == null) {
                throw new Exception();
            }
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

            foreach (TypeDefinition nested in targetType.NestedTypes) {
                relinkerProcessor.Process(nested);
            }
            continue;

            TypeReference? RelinkMapType(TypeReference type) {
                // If it was merged, redirect to destination
                return mergedTypes.GetValueOrDefault(type);
            }

            FieldReference? RelinkMapField(FieldReference field) {
                return fields.GetValueOrDefault(field.ToUID());
            }

            MethodReference? RelinkMapMethod(MethodReference method) {
                return methods.GetValueOrDefault(method.ToUID());
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