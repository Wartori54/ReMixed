using Mono.Cecil;

namespace ReMixed.Processor;

/// <summary>
/// Processes a member.
/// </summary>
/// <typeparam name="T">The member type it targets.</typeparam>
public interface IProcessor<in T> where T : IMemberDefinition {
    public bool Applies(T target);

    public void Process(T target);
}