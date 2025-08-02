using Mono.Cecil;

namespace ReMixed.Processor;

/// <summary>
/// Generates a member from an existing one.
/// </summary>
/// <typeparam name="T">The member type it targets.</typeparam>
/// <typeparam name="TDest">The location to put the generated elements in.</typeparam>
public interface IGenerator<in T, in TDest> where T : IMemberDefinition {
    public bool Applies(T target);

    public void Process(T target, TDest container);
}
