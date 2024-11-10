using System;
using Mono.Cecil;

namespace ReMixed.Transformer;

public interface ITransformer<in TPatch, in TTarget> where TPatch : IMemberDefinition where TTarget : IMemberDefinition {
    
    int Pass { get; }
    
    /// <summary>
    /// Verifies whether the transformer target the current mixin member.
    /// </summary>
    /// <param name="memberDef">The assigned member.</param>
    /// <returns>Whether the transformer has to transform this member.</returns>
    /// <remarks>If the return value is true, `GetTargetPredicate` will be called right after.</remarks>
    bool AppliesTo(TPatch memberDef);

    /// <summary>
    /// Obtains the predicate to find target members for this transformer for the given member source.
    /// </summary>
    /// <param name="memberDef">The member source we are working with.</param>
    /// <returns>The predicate.</returns>
    // Impl details: Ideally this call should not exist, and it would be a nullable out parameter on the `AppliesTo`
    // but because `out` parameters forbid contravariance we cannot use it, and must resort to using return values.
    Predicate<TTarget> GetTargetPredicate(TPatch memberDef);
    
    /// <summary>
    /// Called once per assigned target member, performs the transformation.
    /// </summary>
    /// <param name="memberDefSource">The assigned source member.</param>
    /// <param name="memberDefTarget">The assigned target member.</param>
    void Perform(TPatch memberDefSource, TTarget memberDefTarget);
}
