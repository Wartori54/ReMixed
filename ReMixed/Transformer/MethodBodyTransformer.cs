using System;
using Mono.Cecil;

namespace ReMixed.Transformer;

public abstract class MethodBodyTransformer : ITransformer<MethodDefinition, MethodDefinition> {
    protected MethodPatchContext Context { get; }
    
    protected abstract bool MultiTarget { get; }

    public MethodBodyTransformer(MethodPatchContext context) {
        Context = context;
    }

    public bool AppliesTo(MethodDefinition memberDef) {
        throw new NotImplementedException();
    }
    public Predicate<MethodDefinition> GetTargetPredicate(MethodDefinition memberDef) {
        throw new NotImplementedException();
    }

    public void Perform(MethodDefinition sourceMethod, MethodDefinition methodDef) {
        PatchableMethodDefinition patchableMethodDefinition = PatchableMethodDefinition.FromMethodDef(methodDef);
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
    public abstract bool SeekTarget(MethodPatchContext.Positioner cursor);

    // TODO: Docs
    public abstract void PerformMethod(MethodPatchContext.Cursor cursor, PatchableMethodDefinition patchableMethodDefinition, MethodReference sourceMethod);
}
