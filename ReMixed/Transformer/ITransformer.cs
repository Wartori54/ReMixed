using System;
using Mono.Cecil;

namespace ReMixed.Transformer;

public interface ITransformer<in T> where T : IMemberDefinition {
    /// <summary>
    /// Verifies whether the deserialized data is valid.
    /// </summary>
    /// <param name="memberDef">The assigned member.</param>
    /// <returns>The validity of the current state.</returns>
    bool Validate(T memberDef);
    /// <summary>
    /// Called once per assigned target member, performs the transformation.
    /// </summary>
    /// <param name="memberDef">The assigned member.</param>
    void Perform(T memberDef);
}

public abstract class MethodTransformer : ITransformer<MethodDefinition> {
    protected IPatchContext Context { get; }
    
    protected abstract bool MultiTarget { get; }

    public MethodTransformer(IPatchContext context) {
        Context = context;
    }
    
    public abstract bool Validate(MethodDefinition memberDef);

    public void Perform(MethodDefinition methodDef) {
        IPatchContext.Cursor cursor = Context.ContextCursor;
        if (MultiTarget) {
            while (SeekTarget(cursor)) {
                PerformMethod(methodDef, cursor);
            }

            return;
        }

        if (!SeekTarget(cursor))
            throw new Exception("Single-target MethodTransformer impl did not throw when no target could be found!");
        PerformMethod(methodDef, cursor);
    }

    /// <summary>
    /// Moves the cursor to the next target.
    /// </summary>
    /// <param name="cursor">The cursor.</param>
    /// <returns>Whether the next target was found for multi-target transformers and true or an exception for single-target transformers.</returns>
    /// <remarks>It is a mistake to return false in a single-target transformer, a descriptive exception should be thrown instead.</remarks>
    public abstract bool SeekTarget(IPatchContext.Cursor cursor);

    /// <summary>
    /// Does the transformation for the current cursor position.
    /// </summary>
    /// <param name="methodDef">The assigned MethodDefinition</param>
    /// <param name="cursor">The positioned cursor.</param>
    public abstract void PerformMethod(MethodDefinition methodDef, IPatchContext.Cursor cursor);
}
