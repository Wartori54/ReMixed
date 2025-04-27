using Mono.Cecil;

namespace ReMixed.Processor;

public interface IProcessor<in T> where T : IMemberDefinition {
    public bool Applies(T target);

    public void Process(T target);
}