using System;
using System.Collections.Generic;
using System.Linq;
using Mono.Cecil;
using ReMixed.Positioning;

namespace ReMixed.Transformer;

public abstract class MethodBodyTransformer<TPatchData> : ITransformer<MethodDefinition, MethodDefinition, TPatchData> {
    protected MethodPatchContext Context { get; }
    
    protected abstract bool MultiTarget { get; }

    public MethodBodyTransformer(MethodPatchContext context) {
        Context = context;
    }

    public abstract bool AppliesTo(MethodDefinition memberDef, out TPatchData args);
    
    public abstract Predicate<MethodDefinition> GetTargetPredicate(MethodDefinition memberDef, TPatchData args);

    public void Perform(MethodDefinition sourceMethod, MethodDefinition methodDef, TPatchData args) {
        PatchableMethodDefinition patchableMethodDefinition = PatchableMethodDefinition.FromMethodDef(methodDef);
        MethodPatchContext.Positioner pos = patchableMethodDefinition.AcquirePositioner();
        if (!SeekTarget(pos)) { // If no matches are found, in either case, throw
            if (!MultiTarget)
                throw new Exception("Single-target MethodTransformer impl did not throw when no target could be found");
            else
                throw new Exception("Multi-target MethodTransformer didn't find any matches, check your parameters!");
        }
        // TODO: What about other blob types
        PerformMethod(patchableMethodDefinition.AcquireCursorFromBlob(pos), patchableMethodDefinition, sourceMethod, args);
        // But if this is multi-target, just continue.
        if (!MultiTarget) return;
        
        while (SeekTarget(pos)) {
            PerformMethod(patchableMethodDefinition.AcquireCursorFromBlob(pos), patchableMethodDefinition, sourceMethod, args);
        }
    }

    /// <summary>
    /// Moves the cursor to the next target.
    /// </summary>
    /// <param name="cursor">The cursor.</param>
    /// <returns>Whether the next target was found for multi-target transformer, or either true or an exception for single-target transformers.</returns>
    /// <remarks>It is a mistake to return false in a single-target transformer, a descriptive exception should be thrown instead.</remarks>
    public abstract bool SeekTarget(MethodPatchContext.Positioner cursor);

    // TODO: Docs
    public abstract void PerformMethod(MethodPatchContext.Cursor cursor, PatchableMethodDefinition patchableMethodDefinition, MethodReference sourceMethod, TPatchData args);
}

public abstract class AttributeTargetedMethodBodyTransformer<TPatchData>(MethodPatchContext context, TypeReference targetAttribute) : MethodBodyTransformer<ExpandableSlot<Dictionary<CustomAttribute, MethodReference?>?, TPatchData>>(context) where TPatchData : new() {
    public override sealed bool AppliesTo(MethodDefinition memberDef, out ExpandableSlot<Dictionary<CustomAttribute, MethodReference?>?, TPatchData> args) {
        args = new ExpandableSlot<Dictionary<CustomAttribute, MethodReference?>?, TPatchData>();
        Dictionary<CustomAttribute, MethodReference?> foundAttributes = [];
        foreach (CustomAttribute customAttribute in memberDef.CustomAttributes) {
            if (ILPatcher.TypeReferenceEqual(customAttribute.AttributeType, targetAttribute)) {
                foundAttributes.Add(customAttribute, null);
            }
            // for (TypeReference? typeRef = customAttribute.AttributeType; typeRef != null; typeRef = typeRef.GetBaseTypeCached())
            //     if (customAttribute.AttributeType.FullName == typeof(MethodTargetAttribute).GetCecilFullName()) {
            //         foundAttributes.Add(customAttribute);
            //         break;
            //     }
        }

        if (foundAttributes.Count == 0) {
            args.First = null;
            return false;
        }
        args.First = foundAttributes;
        return true;
    }

    public override sealed Predicate<MethodDefinition> GetTargetPredicate(MethodDefinition memberDef, ExpandableSlot<Dictionary<CustomAttribute, MethodReference?>?, TPatchData> args) {
        if (args.First == null) throw new InvalidOperationException();
        return m => args.First!.Any(kvp => MethodMatchPredicate(m, kvp, args));
    }

    protected virtual bool MethodMatchPredicate(MethodDefinition m, KeyValuePair<CustomAttribute, MethodReference?> kvp, ExpandableSlot<Dictionary<CustomAttribute, MethodReference?>?, TPatchData> args) {
        CustomAttribute attr = kvp.Key;
        // This is always guaranteed, otherwise the attribute is malformed.
        string targetName = (attr.ConstructorArguments[0].Value as string)!;

        if (!Qualifies(targetName)) return false;

        Dictionary<CustomAttribute, MethodReference?> foundAttributes = args.First!;

        if (foundAttributes[attr] != null) throw new InvalidOperationException();
        foundAttributes[attr] = m;
        return true;

        bool Qualifies(string name) {
            // TODO: Improve overload targeting
            if (targetName.Contains('.'))
                return name == m.FullName;
            else
                return name == m.Name;    
        }
    }

    public override sealed void PerformMethod(MethodPatchContext.Cursor cursor, PatchableMethodDefinition patchableMethodDefinition, MethodReference sourceMethod, ExpandableSlot<Dictionary<CustomAttribute, MethodReference?>?, TPatchData> args) {
        if (args.First == null) throw new InvalidOperationException();
        CustomAttribute? attr = null;
        // TODO: Maybe use a reversible dict
        foreach (KeyValuePair<CustomAttribute, MethodReference?> kvp in args.First) {
            if (kvp.Value != null && kvp.Value == patchableMethodDefinition.Reference) {
                attr = kvp.Key;
                break;
            }
        }
        if (attr == null) throw new InvalidOperationException();
        PerformMethod(cursor, patchableMethodDefinition, sourceMethod, attr, args.Second);
    }

    public abstract void PerformMethod(MethodPatchContext.Cursor cursor, PatchableMethodDefinition patchableMethodDefinition, MethodReference sourceMethod, CustomAttribute attribute, TPatchData subArgs);
}

public abstract class AttributePositionedMethodBodyTransformer(MethodPatchContext context, TypeReference targetAttribute) : AttributeTargetedMethodBodyTransformer<Dictionary<string, List<CustomAttribute>>>(context, targetAttribute) {

    protected override bool MethodMatchPredicate(MethodDefinition m, KeyValuePair<CustomAttribute, MethodReference?> kvp, ExpandableSlot<Dictionary<CustomAttribute, MethodReference?>?, Dictionary<string, List<CustomAttribute>>> foundAttributes) {
        if (!base.MethodMatchPredicate(m, kvp, foundAttributes)) return false;

        CustomAttribute attr = kvp.Key;
        
        
        return true;
    }

    public override void PerformMethod(MethodPatchContext.Cursor cursor, PatchableMethodDefinition patchableMethodDefinition, MethodReference sourceMethod, CustomAttribute attribute, Dictionary<string, List<CustomAttribute>> args) {
        throw new NotImplementedException();
    }
    
}