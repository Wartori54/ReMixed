using System;
using System.Collections.Generic;
using System.Linq;
using Mono.Cecil;
using ReMixed.Injection;
using ReMixed.MethodAttribute;
using ReMixed.Positioning;

namespace ReMixed.Transformer;

public abstract class MethodBodyTransformer : ITransformer<MethodDefinition, MethodDefinition> {
    protected MethodPatchContext Context { get; }
    
    protected abstract bool MultiTarget { get; }

    public MethodBodyTransformer(MethodPatchContext context) {
        Context = context;
    }

    public int Pass => 0;

    public abstract bool AppliesTo(MethodDefinition methodDef);
    
    public abstract Predicate<MethodDefinition> GetTargetPredicate(MethodDefinition methodDef);
    
    public void Perform(MethodDefinition sourceMethod, MethodDefinition methodDef) {
        PatchableMethodDefinition patchableMethodDefinition = PatchableMethodDefinition.FromMethodDef(methodDef, Context);
        PerformInner(patchableMethodDefinition, sourceMethod);
    }

    protected virtual void PerformInner(PatchableMethodDefinition patchableMethodDefinition, MethodReference sourceMethod) {
        MethodPatchContext.Positioner pos = patchableMethodDefinition.AcquirePositioner();
        if (!SeekTarget(pos)) { // If no matches are found, in either case, throw
            if (!MultiTarget)
                throw new Exception("Single-target MethodTransformer impl did not throw when no target could be found");
            else
                throw new Exception("Multi-target MethodTransformer didn't find any matches, check your parameters!");
        }
        // TODO: What about other blob types
        PerformMethod(patchableMethodDefinition.AcquireCursorFromBlob(pos), patchableMethodDefinition, sourceMethod);
        // But if this is multi-target, just continue.
        if (!MultiTarget) return;
        
        while (SeekTarget(pos)) {
            PerformMethod(patchableMethodDefinition.AcquireCursorFromBlob(pos), patchableMethodDefinition, sourceMethod);
        }
    }

    /// <summary>
    /// Moves the cursor to the next target.
    /// </summary>
    /// <param name="cursor">The cursor.</param>
    /// <returns>Whether the next target was found for multi-target transformer, or either true or an exception for single-target transformers.</returns>
    /// <remarks>It is a mistake to return false in a single-target transformer, a descriptive exception should be thrown instead.</remarks>
    protected abstract bool SeekTarget(MethodPatchContext.Positioner cursor);

    // TODO: Docs
    protected abstract void PerformMethod(MethodPatchContext.Cursor cursor, PatchableMethodDefinition patchableMethodDefinition, MethodReference sourceMethod);
}

public abstract class AttributeTargetedMethodBodyTransformer(MethodPatchContext context, TypeReference targetAttribute) : MethodBodyTransformer(context) {
    protected override bool MultiTarget => true;

    protected readonly Dictionary<MethodTargetAttribute, MethodReference> AttributeOwners = new();

    private Dictionary<MethodTargetAttribute, MethodReference?>? foundAttributes;

    protected Dictionary<MethodTargetAttribute, MethodReference?>? FoundAttributes {
        get {
            if (revFoundAttributes != null) throw new InvalidOperationException("Tried to access old dict after accessing reverse");
            
            return foundAttributes;
        }
        set => foundAttributes = value;
    }

    private Dictionary<MethodReference, List<MethodTargetAttribute>>? revFoundAttributes = null;
    protected Dictionary<MethodReference, List<MethodTargetAttribute>> RevFoundAttributes {
        get {
            if (revFoundAttributes != null) return revFoundAttributes;
            if (FoundAttributes == null) throw new InvalidOperationException();
            revFoundAttributes = new Dictionary<MethodReference, List<MethodTargetAttribute>>();

            foreach ((MethodTargetAttribute attr, MethodReference? method) in FoundAttributes) {
                if (method == null) throw new InvalidOperationException();
                if (!revFoundAttributes.TryGetValue(method, out List<MethodTargetAttribute>? attrs)) {
                    attrs = new List<MethodTargetAttribute>();
                    revFoundAttributes[method] = attrs;
                }
                attrs.Add(attr);
            }

            return revFoundAttributes;
        }
    }
    public override bool AppliesTo(MethodDefinition methodDef) {
        FoundAttributes = [];
        foreach (CustomAttribute customAttribute in methodDef.CustomAttributes) {
            if (ILPatcher.TypeReferenceEqual(customAttribute.AttributeType, targetAttribute)) {
                MethodTargetAttribute methodTargetAttribute = customAttribute.Instantiate() as MethodTargetAttribute 
                                                              ?? throw new Exception($"Couldn't instantiate {nameof(MethodTargetAttribute)} for attr {customAttribute.AttributeType.FullName} for method {methodDef.FullName}");
                FoundAttributes.Add(methodTargetAttribute, null);
                AttributeOwners.Add(methodTargetAttribute, methodDef);
            }
        }

        if (FoundAttributes.Count == 0) {
            FoundAttributes = null;
            return false;
        }
        return true;
    }

    public override sealed Predicate<MethodDefinition> GetTargetPredicate(MethodDefinition methodDef) {
        if (FoundAttributes == null) throw new InvalidOperationException();
        // TODO: Maybe pair via dict instead of exhaustive search
        return m => FoundAttributes!.Any(kvp => MethodMatchPredicate(m, kvp));
    }

    protected virtual bool MethodMatchPredicate(MethodDefinition methodTarget, KeyValuePair<MethodTargetAttribute, MethodReference?> kvp) {
        MethodTargetAttribute attr = kvp.Key;
        
        if (!Qualifies(attr.MethodTarget)) return false;

        if (FoundAttributes![attr] != null) throw new InvalidOperationException();
        FoundAttributes[attr] = methodTarget;
        return true;

        bool Qualifies(string name) {
            // TODO: Improve overload targeting
            if (name.Contains('.'))
                return name == methodTarget.FullName;
            else
                return name == methodTarget.Name;    
        }
    }

    protected override sealed void PerformMethod(MethodPatchContext.Cursor cursor, PatchableMethodDefinition patchableMethodDefinition, MethodReference sourceMethod) {
        if (FoundAttributes == null) throw new InvalidOperationException();
        // We get all the attributes that target our patching method
        List<MethodTargetAttribute> attrs = RevFoundAttributes[patchableMethodDefinition.Reference];
        // But we have to filter out all the ones that don't come from our source method
        MethodTargetAttribute? targetAttr = null;
        foreach (MethodTargetAttribute attr in attrs) {
            if (AttributeOwners[attr] == sourceMethod)
                targetAttr = attr;
        }
        if (targetAttr == null) throw new InvalidOperationException();

        // This way finally we can call PerformMethod with the useful attribute
        PerformMethod(cursor, patchableMethodDefinition, sourceMethod, targetAttr);
    }

    protected abstract void PerformMethod(MethodPatchContext.Cursor cursor, PatchableMethodDefinition patchableMethodDefinition, MethodReference sourceMethod, MethodTargetAttribute attribute);
}

public abstract class AttributePositionedMethodBodyTransformer(MethodPatchContext context, TypeReference targetAttribute) : AttributeTargetedMethodBodyTransformer(context, targetAttribute) {

    protected Dictionary<string, AtAttribute>? FoundAts;
    private readonly TypeReference targetAttribute = targetAttribute;

    public override bool AppliesTo(MethodDefinition memberDef) {
        if (!base.AppliesTo(memberDef)) return false;
        
        FoundAts = new Dictionary<string, AtAttribute>();

        foreach (CustomAttribute customAttribute in memberDef.CustomAttributes) {
            if (ILPatcher.TypeReferenceEqual(customAttribute.AttributeType, Context.Platform.ThisCecilDefs.AtAttribute)) {
                AtAttribute atAttribute = customAttribute.Instantiate() as AtAttribute 
                                           ?? throw new Exception($"Couldn't instantiate {nameof(AtAttribute)} for attr {customAttribute.AttributeType.FullName} for method {memberDef.FullName}");
                if (!FoundAts.TryAdd(atAttribute.Id, atAttribute))
                    throw new Exception($"Duplicated id {atAttribute.Id} in {nameof(AtAttribute)} for method {memberDef.FullName}");
            }
        }

        // If there's no positioners do not apply
        if (FoundAts.Count == 0) throw new Exception($"Could not find any {nameof(AtAttribute)} for method {memberDef.FullName} with transformer attribute {targetAttribute.FullName}");
        return true;
    }

    protected override bool MethodMatchPredicate(MethodDefinition methodTarget, KeyValuePair<MethodTargetAttribute, MethodReference?> kvp) {
        if (!base.MethodMatchPredicate(methodTarget, kvp)) return false;

        // If this attribute is not for us, just skip
        if (kvp.Key is not MethodPositionedAttribute positionedAttribute) return false;
        string[] ats = positionedAttribute.At;
        if (ats.Length == 0) throw new Exception($"Found no Ids in the {nameof(MethodPositionedAttribute.At)} property");
        
        return true;
    }

    protected override void PerformInner(PatchableMethodDefinition patchableMethodDefinition, MethodReference sourceMethod) {
        List<MethodTargetAttribute> targetAttributes = RevFoundAttributes[patchableMethodDefinition.Reference];
        MethodPatchContext.Positioner pos = patchableMethodDefinition.AcquirePositioner();
        foreach (MethodTargetAttribute methodTargetAttribute in targetAttributes) {
            // This has to always succeed, since this transformer returned false in the target predicate finder if the targeted method by an attribute was not of our interest
            MethodPositionedAttribute methodPositionedAttribute = (MethodPositionedAttribute)methodTargetAttribute;
            Injector injector = Context.Platform.InjectorRegistry.CreateForAttribute(methodPositionedAttribute, Context);
            foreach (string atId in methodPositionedAttribute.At) {
                AtAttribute atAttribute = FoundAts![atId];
                Positioner positioner = Positioner.FromAttribute(atAttribute);
                positioner.Do(pos);
                
                injector.Inject(patchableMethodDefinition.AcquireCursorFromBlob(pos), patchableMethodDefinition, sourceMethod);
            }
        }
    }

    protected override sealed bool SeekTarget(MethodPatchContext.Positioner cursor) {
        throw new NotSupportedException();
    }

    protected override void PerformMethod(MethodPatchContext.Cursor cursor, PatchableMethodDefinition patchableMethodDefinition, MethodReference sourceMethod, MethodTargetAttribute attribute) {
        throw new NotSupportedException();
    }
    
}