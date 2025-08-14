using System;
using System.Collections.Generic;
using Mono.Cecil;
using Mono.Collections.Generic;

namespace ReMixed.Transformer;


public interface ITransformerFactory<in TPatch, TTarget> where TPatch : IMemberDefinition where TTarget : IMemberDefinition {
    int Pass { get; }
    
    /// <summary>
    /// Verifies whether the transformer targets the current mixin member. And returns the targetable instances from a collection.
    /// </summary>
    /// <param name="memberDef">The assigned member.</param>
    /// <param name="targets">The collection of targets.</param>
    /// <returns>The targettable instances from <paramref name="targets"/> or null if <paramref name="memberDef"/> does not apply.</returns>
    IEnumerable<TTarget>? AppliesTo(TPatch memberDef, Collection<TTarget> targets);
    
    ITransformer<TPatch, TTarget> For(TPatch patch, TTarget target);

    /// <summary>
    /// Obtains the predicate to find target members for this transformer for the given member source.
    /// </summary>
    /// <param name="memberDef">The member source we are working with.</param>
    /// <returns>The predicate.</returns>
    // Impl details: Ideally this call should not exist, and it would be a nullable out parameter on the `AppliesTo`
    // but because `out` parameters forbid contravariance we cannot use it, and must resort to using return values.
    // Predicate<TTarget> GetTargetPredicate(TPatch memberDef);
}

public interface ITransformer<in TPatch, in TTarget> where TPatch : IMemberDefinition where TTarget : IMemberDefinition {
    /// <summary>
    /// Called once per assigned target member, performs the transformation.
    /// </summary>
    /// <param name="memberDefSource">The assigned source member.</param>
    /// <param name="memberDefTarget">The assigned target member.</param>
    void Perform(TPatch memberDefSource, TTarget memberDefTarget);
}
