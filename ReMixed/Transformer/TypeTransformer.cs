using System;
using Mono.Cecil;

namespace ReMixed.Transformer;

public abstract class TypeTransformer : ITransformer<TypeDefinition, TypeDefinition> {
    private readonly ITransformer<FieldDefinition, FieldDefinition>? fieldTransformer;
    private readonly ITransformer<PropertyDefinition, PropertyDefinition>? propertyTransformer;
    private readonly ITransformer<EventDefinition, EventDefinition>? eventTransformer;
    private readonly ITransformer<MethodDefinition, MethodDefinition>? methodTransformer;
    private readonly ITransformer<TypeDefinition, TypeDefinition>? typeTransformer;


    public abstract int Pass { get; }
    public abstract bool AppliesTo(TypeDefinition memberDef);
    public abstract Predicate<TypeDefinition> GetTargetPredicate(TypeDefinition memberDef);


    public TypeTransformer(ITransformer<FieldDefinition, FieldDefinition>? fieldTransformer = null, ITransformer<PropertyDefinition, PropertyDefinition>? propertyTransformer = null, ITransformer<EventDefinition, EventDefinition>? eventTransformer = null, ITransformer<MethodDefinition, MethodDefinition>? methodTransformer = null, ITransformer<TypeDefinition, TypeDefinition>? typeTransformer = null) {
        this.fieldTransformer = fieldTransformer;
        this.propertyTransformer = propertyTransformer;
        this.eventTransformer = eventTransformer;
        this.methodTransformer = methodTransformer;
        this.typeTransformer = typeTransformer;
    }


    public void Perform(TypeDefinition memberDefSource, TypeDefinition memberDefTarget) {
        
    }
}