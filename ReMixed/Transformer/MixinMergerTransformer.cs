using System;
using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.CodeAnalysis;
using Mono.Cecil;
using Mono.Collections.Generic;
using MonoMod.Utils;

namespace ReMixed.Transformer;

public class MixinMergerTransformer : ITransformer<TypeDefinition, TypeDefinition> {
    public int Pass => -1;

    private readonly PatchPlatform platform;
    
    private readonly Dictionary<TypeReference, TypeIndex> mixinTypeIndex = new();

    // Maps FullNames to all members that have a relation to it that have been merged
    // Fields are attached to their type fullname
    // Properties are attached to their 
    private readonly Dictionary<string, List<IMemberDefinition>> relinkTargets = new();

    // Speed purposes, maps pre-relinked methods to properties
    private readonly Dictionary<MethodDefinition, PropertyDefinition> methodToProperty = new();
    
    // Speed purposes, maps pre-relinked methods to events
    private readonly Dictionary<MethodDefinition, EventDefinition> methodToEvent = new();
    
    // Used for the post-copy pass to fix methods, filled in CopyMethod
    private readonly Dictionary<PropertyDefinition, (MethodDefinition get, MethodDefinition set, Collection<MethodDefinition> other)> relinkedProperties = new();
    
    private readonly Dictionary<TypeDefinition, TypeDefinition> mergedTypes = new();
    
    private readonly Dictionary<FieldDefinition, FieldDefinition> copiedFields = new();
    private readonly Dictionary<PropertyDefinition, PropertyDefinition> copiedProperties = new();
    private readonly Dictionary<EventDefinition, EventDefinition> copiedEvents = new();
    private readonly Dictionary<MethodDefinition, MethodDefinition> copiedMethods = new();
    
    public Dictionary<TypeDefinition, TypeDefinition> MergedTypes => mergedTypes;
    public Dictionary<FieldDefinition, FieldDefinition> CopiedFields => copiedFields;
    public Dictionary<PropertyDefinition, PropertyDefinition> CopiedProperties => copiedProperties;
    public Dictionary<EventDefinition, EventDefinition> CopiedEvents => copiedEvents;
    public Dictionary<MethodDefinition, MethodDefinition> CopiedMethods => copiedMethods;
    
    
    private MixinAttribute? mixinAttribute;
    private TypeReference? targetMixinType;

    public MixinMergerTransformer(PatchPlatform patchPlatform) {
        platform = patchPlatform;
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
        if (memberDefSource.GenericParameters.Count != memberDefTarget.GenericParameters.Count) throw new Exception($"Generic parameter count mismatch in {memberDefSource.FullName}->{memberDefTarget.FullName}");
        for (int i = 0; i < memberDefTarget.GenericParameters.Count; i++) {
            if (!memberDefSource.GenericParameters[i].GenericEquals(memberDefTarget.GenericParameters[i])) throw new Exception($"Generic parameter {i} in {memberDefSource.FullName} does not match {memberDefTarget.FullName}");
        }

        // Ignored
        // foreach (SecurityDeclaration securityDeclaration in memberDefTarget.SecurityDeclarations) {
        //     
        // }
        
        mergedTypes[memberDefSource] = memberDefTarget;
        
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
        
        // TODO: RelinkTarget pass
        // foreach ((string typeTarget, List<IMemberDefinition> members) in relinkTargets) {
        //     if (!mergedTypes.TryGetValue(typeTarget, out TypeDefinition? mergedType)) continue;
        //     foreach (IMemberDefinition member in members) {
        //         switch (member) {
        //             case FieldDefinition fieldDefinition:
        //                 fieldDefinition.FieldType = mergedType;
        //                 break;
        //             default:
        //                 throw new NotSupportedException();
        //                 break;
        //         }
        //     }
        // }
    }

    private void CopyField(TypeDefinition dest, FieldDefinition src) {
        if (IndexType(dest).Identifiers.Contains(src.Name)) throw new Exception($"Field with identifier {src.FullName} already present in type {dest.FullName}");
        FieldDefinition copy = src.Clone();
        // AddToRelinkTargets(copy.FieldType, copy);
        dest.Fields.Add(copy);
        copiedFields[src] = copy;
    }

    private void CopyProperty(TypeDefinition dest, PropertyDefinition src) {
        if (IndexType(dest).Identifiers.Contains(src.Name)) throw new Exception($"Property with identifier {src.FullName} already present in type {dest.FullName}");
        PropertyDefinition copy = src.Clone();
        if (copy.GetMethod != null) methodToProperty[copy.GetMethod] = copy;
        if (copy.SetMethod != null) methodToProperty[copy.SetMethod] = copy;
        foreach (MethodDefinition otherM in copy.OtherMethods) {
            methodToProperty[otherM] = copy;
        }
        // AddToRelinkTargets(copy.PropertyType, copy);
        dest.Properties.Add(copy);
        copiedProperties[src] = copy;
    }

    private void CopyEvent(TypeDefinition dest, EventDefinition src) {
        if (IndexType(dest).Identifiers.Contains(src.Name)) throw new Exception($"Event with identifier {src.FullName} already present in type {dest.FullName}");
        EventDefinition copy = src.Clone();
        if (copy.AddMethod != null) methodToEvent[copy.AddMethod] = copy;
        if (copy.RemoveMethod != null) methodToEvent[copy.RemoveMethod] = copy;
        if (copy.InvokeMethod != null) methodToEvent[copy.InvokeMethod] = copy;
        foreach (MethodDefinition otherM in copy.OtherMethods) {
            methodToEvent[otherM] = copy;
        }
        // AddToRelinkTargets(copy.EventType, copy);
        dest.Events.Add(copy);
        copiedEvents[src] = copy;
    }

    private void CopyMethod(TypeDefinition dest, MethodDefinition src) {
        if (IndexType(dest).Identifiers.Contains(src.Name)) throw new Exception($"Method with identifier {src.FullName} already present in type {dest.FullName}");
        MethodDefinition copy = src.Clone();
        // Relink: ret value, parameters, overrides, gparameters, mbody's (this param, variables)
        dest.Methods.Add(copy);
        copiedMethods[src] = copy;
    }

    private void ApplyInterface(TypeDefinition dest, InterfaceImplementation src) {
        InterfaceImplementation copy = src.Clone();
        dest.Interfaces.Add(copy);
    }

    private void AddToRelinkTargets(TypeReference typeRef, IMemberDefinition member) {
        if (typeRef is TypeSpecification) throw new NotImplementedException();
        if (typeRef is GenericParameter) throw new NotSupportedException();
        if (!relinkTargets.TryGetValue(typeRef.FullName, out List<IMemberDefinition>? members)) {
            relinkTargets[typeRef.FullName] = members = [];
        }
        members.Add(member);
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
}