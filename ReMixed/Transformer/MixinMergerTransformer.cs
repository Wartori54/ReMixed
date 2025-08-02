using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using Microsoft.CodeAnalysis;
using Mono.Cecil;
using Mono.Cecil.Rocks;
using Mono.Collections.Generic;
using MonoMod.Utils;
using ReMixed.Processor;

namespace ReMixed.Transformer;

/// <summary>
/// Merges a [Mixin] annotated type into its target, simply copying nicely all the members.
/// </summary>
public class MixinMergerTransformer : ITransformer<TypeDefinition, TypeDefinition> {
    public int Pass => -1;

    private readonly PatchPlatform platform;
    
    private readonly Dictionary<TypeReference, TypeIndex> mixinTypeIndex = new();

    // Speed purposes, maps pre-relinked methods to properties
    private readonly Dictionary<MethodDefinition, (PropertyDefinition, MethodInPropType)> methodToProperty = new();
    
    // Speed purposes, maps pre-relinked methods to events
    private readonly Dictionary<MethodDefinition, (EventDefinition, MethodInEventType)> methodToEvent = new();
    
    private readonly Dictionary<FieldRefUID, FieldReference> copiedFields;
    private readonly Dictionary<PropertyRefUID, PropertyDefinition> copiedProperties;
    private readonly Dictionary<EventRefUID, EventDefinition> copiedEvents;
    private readonly Dictionary<MethodRefUID, MethodDefinition> copiedMethods;
    
    // public Dictionary<FieldReference, FieldReference> CopiedFields => copiedFields;
    // public Dictionary<PropertyDefinition, PropertyDefinition> CopiedProperties => copiedProperties;
    // public Dictionary<EventDefinition, EventDefinition> CopiedEvents => copiedEvents;
    // public Dictionary<MethodDefinition, MethodDefinition> CopiedMethods => copiedMethods;
    
    
    private MixinAttribute? mixinAttribute;
    private TypeReference? targetMixinType;

    public MixinMergerTransformer(PatchPlatform patchPlatform, 
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

    // This applies to all mixin types
    public bool AppliesTo(TypeDefinition memberDef) {
        bool applies = false;
        foreach (CustomAttribute customAttribute in memberDef.CustomAttributes) {
            if (!ILPatcher.TypeReferenceEqual(customAttribute.AttributeType, platform.ThisCecilDefs.MixinAttribute)) continue;
            if (mixinAttribute != null) throw new NotSupportedException("Cannot mixin to multiple classes using the same instance");
            mixinAttribute = (MixinAttribute) customAttribute.Instantiate()!;
            // The following ImportReference should not fail since you will need an asm ref to embed the type in a custom attr in the first place
            targetMixinType = memberDef.Module.ImportReference(mixinAttribute.Target);
            applies = true;
        }
        return applies;
    }
    
    // Find all classes that have the attribute targeting the current class
    public Predicate<TypeDefinition> GetTargetPredicate(TypeDefinition memberDef) {
        return Util.ToOneShot<TypeDefinition>(type => ILPatcher.TypeReferenceEqual(targetMixinType ?? throw new UnreachableException(), type));
    }
    
    // source -> mixin, target -> type targeted by the mixin
    public void Perform(TypeDefinition memberDefSource, TypeDefinition memberDefTarget) {
        IndexType(memberDefTarget); // Cache it before starting
        foreach (FieldDefinition field in memberDefSource.Fields) {
            CopyField(memberDefTarget, field);
        }
        
        foreach (PropertyDefinition property in memberDefSource.Properties) {
            CopyProperty(memberDefTarget, property);
        }
        
        foreach (EventDefinition @event in memberDefSource.Events) {
            CopyEvent(memberDefTarget, @event);
        }
        
        foreach (MethodDefinition method in memberDefSource.Methods) {
            CopyMethod(memberDefTarget, method);
        }
        
        // Nested types are handled as separate types
        // foreach (TypeDefinition typeDefinition in memberDefTarget.NestedTypes) {
        //     CopyNested(memberDefSource, typeDefinition);
        // }
        
        foreach (InterfaceImplementation interfaceImplementation in memberDefSource.Interfaces) {
            ApplyInterface(memberDefTarget, interfaceImplementation);
        }

        // Just verify that it matches, since it doesn't really make sense to "merge" them
        if (memberDefSource.GenericParameters.Count != memberDefTarget.GenericParameters.Count) 
            throw new Exception($"Generic parameter count mismatch in {memberDefSource.FullName}->{memberDefTarget.FullName} ({memberDefSource.GenericParameters.Count}->{memberDefTarget.GenericParameters.Count})");
        for (int i = 0; i < memberDefTarget.GenericParameters.Count; i++) {
            if (!memberDefSource.GenericParameters[i].GenericEquals(memberDefTarget.GenericParameters[i])) throw new Exception($"Generic parameter {i} in {memberDefSource.FullName} does not match {memberDefTarget.FullName}");
        }

        // Ignored
        // foreach (SecurityDeclaration securityDeclaration in memberDefTarget.SecurityDeclarations) {
        //     
        // }
        
        // foreach ((PropertyDefinition relinkedProperty, (MethodDefinition relinkedGet, MethodDefinition relinkedSet, Collection<MethodDefinition> other)) in relinkedProperties) {
        //     if (relinkedProperty.GetMethod != null && relinkedProperty.GetMethod.DeclaringType != memberDefSource) {
        //         relinkedProperty.GetMethod = relinkedGet;
        //     }
        //     
        //     if (relinkedProperty.SetMethod != null && relinkedProperty.SetMethod.DeclaringType != memberDefSource) {
        //         relinkedProperty.SetMethod = relinkedSet;
        //     }
        //
        //     Collection<MethodDefinition> oldOtherMembers = relinkedProperty.OtherMethods;
        //     relinkedProperty.OtherMethods.Clear();
        //     
        //     // O(n^2) solution lets go!!
        //     foreach (MethodDefinition oldOtherMethod in oldOtherMembers) {
        //         foreach (MethodDefinition newOtherMethod in other) {
        //             if (oldOtherMethod.Name == newOtherMethod.Name) {
        //                 relinkedProperty.OtherMethods.Add(newOtherMethod);
        //                 break;
        //             }
        //         }
        //     }
        // }
        
        // Relink the merged type
    }

    private void CopyField(TypeDefinition dest, FieldDefinition src) {
        if (IndexType(dest).Identifiers.Contains(src.Name)) throw new Exception($"Field with identifier {src.FullName} already present in type {dest.FullName}");
        FieldDefinition copy = src.Clone();
        // AddToRelinkTargets(copy.FieldType, copy);
        dest.Fields.Add(copy);
        copiedFields[src.ToUID()] = copy;
    }

    private void CopyProperty(TypeDefinition dest, PropertyDefinition src) {
        if (IndexType(dest).Identifiers.Contains(src.Name)) throw new Exception($"Property with identifier {src.FullName} already present in type {dest.FullName}");
        PropertyDefinition copy = src.Clone();
        if (copy.GetMethod != null) methodToProperty[copy.GetMethod] = (copy, MethodInPropType.Get);
        if (copy.SetMethod != null) methodToProperty[copy.SetMethod] = (copy, MethodInPropType.Set);
        for (int i = 0; i < copy.OtherMethods.Count; i++) {
            methodToProperty[copy.OtherMethods[i]] = (copy, MethodInPropType.Other + i);
        }
        // AddToRelinkTargets(copy.PropertyType, copy);
        dest.Properties.Add(copy);
        copiedProperties[src.ToUID()] = copy;
    }

    private void CopyEvent(TypeDefinition dest, EventDefinition src) {
        if (IndexType(dest).Identifiers.Contains(src.Name)) throw new Exception($"Event with identifier {src.FullName} already present in type {dest.FullName}");
        EventDefinition copy = src.Clone();
        if (copy.AddMethod != null) methodToEvent[copy.AddMethod] = (copy, MethodInEventType.Add);
        if (copy.RemoveMethod != null) methodToEvent[copy.RemoveMethod] = (copy, MethodInEventType.Remove);
        if (copy.InvokeMethod != null) methodToEvent[copy.InvokeMethod] = (copy, MethodInEventType.Invoke);
        for (int i = 0; i < copy.OtherMethods.Count; i++) {
            methodToEvent[copy.OtherMethods[i]] = (copy, MethodInEventType.Other + i);
        }
        // AddToRelinkTargets(copy.EventType, copy);
        dest.Events.Add(copy);
        copiedEvents[src.ToUID()] = copy;
    }

    private void CopyMethod(TypeDefinition dest, MethodDefinition src) {
        // Ignore .ctor specifically
        if (src.Name == ".ctor") {
            // But register the .ctor as moved
            foreach (MethodDefinition destCtor in dest.GetConstructors()) {
                if (destCtor.Parameters.Count != src.Parameters.Count) continue;
                bool match = true;
                for (int i = 0; i < src.Parameters.Count; i++) {
                    if (ILPatcher.TypeReferenceEqual(src.Parameters[i].ParameterType, destCtor.Parameters[i].ParameterType)) continue;
                    match = false;
                }
                if (match) {
                    copiedMethods[src.ToUID()] = destCtor;
                    break;
                }
            }
            return;
        }
        if (IndexType(dest).Identifiers.Contains(src.Name)) throw new Exception($"Method with identifier {src.FullName} already present in type {dest.FullName}");
        MethodDefinition copy = src.Clone();
        // Relink: ret value, parameters, overrides, gparameters, mbody's (this param, variables)
        dest.Methods.Add(copy);
        copiedMethods[src.ToUID()] = copy;
        
        // Also fix the method in the copied prop
        {
            if (methodToProperty.TryGetValue(src, out (PropertyDefinition newProp, MethodInPropType type) v)) {
                switch (v.type) {
                    case MethodInPropType.Get:
                        v.newProp.GetMethod = copy;
                        break;
                    case MethodInPropType.Set:
                        v.newProp.SetMethod = copy;
                        break;
                    case MethodInPropType.Other: // Other and any values higher than it
                    default: {
                        if (v.newProp.OtherMethods.Count <= v.type - MethodInPropType.Other) {
                            v.newProp.OtherMethods.Capacity = v.type - MethodInPropType.Other + 1;
                        }
                        v.newProp.OtherMethods[v.type - MethodInPropType.Other] = copy;
                        break;
                    }
                }
            }
        }

        {
            if (methodToEvent.TryGetValue(src, out (EventDefinition newEvent, MethodInEventType type) v)) {
                switch (v.type) {
                    case MethodInEventType.Add:
                        v.newEvent.AddMethod = copy;
                        break;
                    case MethodInEventType.Remove:
                        v.newEvent.RemoveMethod = copy;
                        break;
                    case MethodInEventType.Invoke:
                        v.newEvent.InvokeMethod = copy;
                        break;
                    case MethodInEventType.Other: // Other and any values higher than it
                    default: {
                        if (v.newEvent.OtherMethods.Count <= v.type - MethodInEventType.Other) {
                            v.newEvent.OtherMethods.Capacity = v.type - MethodInEventType.Other + 1;
                        }
                        v.newEvent.OtherMethods[v.type - MethodInEventType.Other] = copy;
                        break;
                    }
                }
            }
        }
    }

    private void ApplyInterface(TypeDefinition dest, InterfaceImplementation src) {
        InterfaceImplementation copy = src.Clone();
        dest.Interfaces.Add(copy);
    }

    private TypeIndex IndexType(TypeDefinition type) {
        if (mixinTypeIndex.TryGetValue(type, out TypeIndex? index)) {
            return index;
        }
        
        index = new TypeIndex();

        type.ForEachMember(member => index.Identifiers.Add(member.Name));
        mixinTypeIndex[type] = index;
        return index;
    }

    private record TypeIndex {
        public HashSet<string> Identifiers { get; } = new();
        // public HashSet<string> FieldNames { get; } = new();
        // public HashSet<string> MethodNames { get; } = new();
        // public HashSet<string> PropertyNames { get; } = new();
    }

    private enum MethodInPropType {
        Get,
        Set,
        Other,
    }

    private enum MethodInEventType {
        Add,
        Remove,
        Invoke,
        Other,
    }
}