using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.InteropServices;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Collections.Generic;

namespace ReMixed.Processor;

// TODO: Custom attributes
public class RelinkerProcessor : IProcessor<TypeDefinition> {
    private readonly RelinkMap<TypeReference> typeRelinker;
    private readonly RelinkMap<FieldReference> fieldRelinker;
    private readonly RelinkMap<MethodReference> methodRelinker;
    // private readonly RelinkMap<PropertyReference> propertyRelinker;
    // private readonly RelinkMap<EventReference> eventRelinker;

    // Rather than forcing dicts into here, allow delegates to do so, for flexibility
    // (hopefully the runtime will be smart enough to inline stuff here...)
    public delegate T? RelinkMap<T>(T type) where T : MemberReference;
    
    public RelinkerProcessor(
        RelinkMap<TypeReference> typeRelinker,
        RelinkMap<FieldReference> fieldRelinker,
        RelinkMap<MethodReference> methodRelinker
        // RelinkMap<PropertyReference> propertyRelinker,
        // RelinkMap<EventReference> eventRelinker
        ) {
        this.typeRelinker = typeRelinker;
        this.fieldRelinker = fieldRelinker;
        this.methodRelinker = methodRelinker;
        // this.propertyRelinker = propertyRelinker;
        // this.eventRelinker = eventRelinker;
    }
    
    // There isn't any good ways to know if this has to be relinked ahead of time so just
    // return true always and expect the caller to only feed the types which actually have
    // to be relinked.
    public bool Applies(TypeDefinition target) {
        return true;
    }
    
    // Main Entrypoint
    public void Process(TypeDefinition target) {
        foreach (FieldDefinition field in target.Fields) {
            Relink(field);
        }

        foreach (PropertyDefinition property in target.Properties) {
            Relink(property);
        }

        foreach (EventDefinition @event in target.Events) {
            Relink(@event);
        }
        
        foreach (MethodDefinition method in target.Methods) {
            Relink(method);
        }

        foreach (InterfaceImplementation interfaceImpl in target.Interfaces) {
            Relink(interfaceImpl, target);
        }

        foreach (GenericParameter genericParameter in target.GenericParameters) {
            foreach (GenericParameterConstraint constraint in genericParameter.Constraints) {
                constraint.ConstraintType = Relink(constraint.ConstraintType, target);
                RelinkCAs(constraint.CustomAttributes);
            }
            RelinkCAs(genericParameter.CustomAttributes);
        }
        
        RelinkCAs(target.CustomAttributes);
    }

    #region Reference Relinks
    // Reference relinks will have to always clone the reference themselves (if the reference cant be retargeted) since
    // the member cloner only clones definitions
    
    // Overload abuse to basically make it seem dynamic (automatically pull the proper RelinkerMap for each type) while still having static typing
    // Also handle edge cases of possible Reference types
    
    private TypeReference Relink(TypeReference val, IMemberDefinition? ctx) {
        // TypeReferences have two edge cases in their hierarchy, handle those here
        if (val is GenericParameter gParam) { // Try to obtain the already existing generic parameters on the new type, otherwise throw
            if (ctx == null) {
                throw new InvalidOperationException($"Tried to relink {val} with null context, but a context is required!");
            }
            // The generic parameters present in a relink must match to the existing ones in the parent where the generic parameter lives
            // otherwise we would need to add a generic parameter to the type/method and that would invalidate all references
            GenericParameter newGParam = gParam.Type switch {
                // Nested parameters redeclare the generic parameters present in the parent, thus indices are not really owner aware
                GenericParameterType.Type => PairWithParent(gParam, ctx.DeclaringType), // Safe assumption, since we will never relink a type's own generic parameters here
                GenericParameterType.Method => PairWithParent(gParam, (MethodDefinition)ctx), // If this cast fails it's a bug
                _ => throw new ArgumentOutOfRangeException(nameof(val))
            };
            return newGParam;
            // Constraints are handled once the type generics are relinked
            // Note: we may even drop the constraints of a generic parameter since the only relevant information
            // that a generic parameter contains is it's index (once it has been attached somewhere that is)
            // TODO: Check if constrains are invalid
        }
        // Do this a little bit later since the GenericParameter case does not relink at all
        Debug.Assert(val.DeclaringType is not GenericInstanceType);
        if (val is TypeSpecification tSpec) {
            TypeReference relType = Relink(tSpec.ElementType, ctx);
            
            switch (tSpec) {
                case ArrayType arrayType:
                    ArrayType arrT = new(relType, arrayType.Rank);
                    foreach (ArrayDimension dim in arrayType.Dimensions) {
                        arrT.Dimensions.Add(dim);
                    }
                    return arrayType;
                case ByReferenceType:
                    return new ByReferenceType(relType);
                case FunctionPointerType fpType:
                    // Ideally we relink the method reference it holds, unfortunately its private
                    FunctionPointerType newFpType = new() {
                        ReturnType = Relink(fpType.ReturnType, ctx /* i will just assume that the ReturnType cannot be GenericParameter if it's well-formed */ ),
                    };
                    foreach (ParameterDefinition parameter in fpType.Parameters) {
                        newFpType.Parameters.Add(new ParameterDefinition(parameter.Name, parameter.Attributes, Relink(parameter.ParameterType, ctx)));
                    }
                    Debug.Assert(fpType.GenericParameters.Count == 0);
                    return newFpType;
                case GenericInstanceType genericInstanceType:
                    GenericInstanceType relGType = new(relType);
                    foreach (TypeReference genArg in genericInstanceType.GenericArguments) {
                        TypeReference relGenArg = Relink(genArg, ctx);
                        relGType.GenericArguments.Add(relGenArg);
                    }
                    return relGType;
                case OptionalModifierType optionalModifierType:
                    TypeReference newModifierType = Relink(optionalModifierType.ModifierType, ctx);
                    return new OptionalModifierType(newModifierType, relType);
                case PinnedType:
                    return new PinnedType(relType);
                case PointerType:
                    return new PointerType(relType);
                case RequiredModifierType requiredModifierType:
                    newModifierType = Relink(requiredModifierType.ModifierType, ctx);
                    return new RequiredModifierType(newModifierType, relType);
                case SentinelType:
                    return new SentinelType(relType);
                default:
                    throw new ArgumentOutOfRangeException(nameof(val));
            }
            
        }
        return Relink(typeRelinker, val);
    }
    
    private FieldReference Relink(FieldReference val, IMemberDefinition? ctx) {
        TypeReference[] genArgs = RelinkGenericArgsFromDeclTypes(val, ctx);
        
        // FieldReferences are nice, no edge cases
        if (genArgs.Length == 0) {
            return Relink(fieldRelinker, val);
        }
        FieldReference relk = Relink(fieldRelinker, val);
        if (relk == val) return relk; // Cannot Reattach without having removed generic arguments
        relk = relk.CloneAndAttachDeclType(MemberCloner.Clone);
        ReAttachGenericArgumentsLike(relk, genArgs);
        return relk;
    }
    
    private MethodReference Relink(MethodReference val, IMemberDefinition? ctx) {
        if (val is MethodSpecification) {
            // Anything implementing other methodSpec types wont be supported
            if (val is not GenericInstanceMethod gMethod) throw new NotSupportedException($"All {nameof(MethodSpecification)} must be {nameof(GenericInstanceMethod)}!");
            GenericInstanceMethod newGI = new(Relink(gMethod.ElementMethod, ctx));
            Collection<TypeReference> newArgs = newGI.GenericArguments;
            newArgs.Capacity = gMethod.GenericArguments.Count;
            for (int i = 0; i < gMethod.GenericArguments.Count; i++) {
                IMemberDefinition? newCtx = ctx;
                if (gMethod.GenericArguments[i] is GenericParameter gParam) {
                    if (ctx == null) {
                        throw new InvalidOperationException($"Tried to relink {val} with null context but a context is required!");
                    }
                    newCtx = gParam.Type switch {
                        GenericParameterType.Type => ctx,
                        GenericParameterType.Method => ObtainMethodDefAssert(ctx),
                        _ => throw new ArgumentOutOfRangeException(nameof(val))
                    };
                }
                // Assigning to Capacity won't change the actual size, so doing newArgs[i] = ... will throw arg out of range here
                newArgs.Add(Relink(gMethod.GenericArguments[i], newCtx));
                continue;

                IMemberDefinition ObtainMethodDefAssert(IMemberDefinition context) {
                    // I will assume that GenericParameters of type method will only appear in the contexts where we have the definition already
                    // (in method bodies exclusively)
                    // TODO: verify the above
                    Debug.Assert(context is MethodDefinition);
                    return (MethodDefinition)context;
                }
            }
            return newGI;
        }
        // The above condition recursively calls and will ultimately go through this
        TypeReference[] genArgs = RelinkGenericArgsFromDeclTypes(val, ctx);
        if (genArgs.Length == 0) {
            return Relink(methodRelinker, val);
        }
        MethodReference relk = Relink(methodRelinker, val);
        if (relk == val) return relk;
        relk = relk.CloneAndAttachDeclType(MemberCloner.Clone);
        ReAttachGenericArgumentsLike(relk, genArgs);
        return relk;
    }

    // There can't really be property and event references in an assembly
    // Those are here for completeness
    // private PropertyReference Relink(PropertyReference val, IMemberDefinition ctx) {
        // ...
    // }

    // private EventReference Relink(EventReference val, IMemberDefinition ctx) {
        // ...
    // }

    private TR Relink<TR>(RelinkMap<TR> rel, TR val) where TR : MemberReference {
        return rel(val) ?? val; // Null returns are equivalent to "I don't know how to handle it"
    }
    
    #endregion

    #region Definition Relinks
    // Actual definition relinking, basically iterate over everything and relink all refs
    private void Relink(FieldDefinition field) {
        // Fields only need to have this relinked
        field.FieldType = Relink(field.FieldType, field);
        RelinkCAs(field.CustomAttributes);
    }

    private void Relink(PropertyDefinition property) {
        property.PropertyType = Relink(property.PropertyType, property);
        property.GetMethod = ProperResolve(Relink(property.GetMethod, property));
        property.SetMethod = ProperResolve(Relink(property.SetMethod, property));
        for (int i = 0; i < property.OtherMethods.Count; i++) {
            property.OtherMethods[i] = ProperResolve(Relink(property.OtherMethods[i], property));
        }
        
        RelinkCAs(property.CustomAttributes);

        return;
        MethodDefinition ProperResolve(MethodReference mref) {
            if (mref is MethodDefinition mdef) return mdef;
            throw new InvalidOperationException("Improper relink of property, this is a bug!");
        }
    }

    private void Relink(EventDefinition @event) {
        @event.EventType = Relink(@event.EventType, @event);
        // If only you could have arrays of refs
        @event.AddMethod = ProperResolve(Relink(@event.AddMethod, @event));
        @event.RemoveMethod = ProperResolve(Relink(@event.RemoveMethod, @event));
        @event.InvokeMethod = ProperResolve(Relink(@event.InvokeMethod, @event));
        for (int i = 0; i < @event.OtherMethods.Count; i++) {
            @event.OtherMethods[i] = ProperResolve(Relink(@event.OtherMethods[i], @event));
        }
        
        RelinkCAs(@event.CustomAttributes);
        
        return;
        MethodDefinition ProperResolve(MethodReference mref) {
            if (mref is MethodDefinition mdef) return mdef;
            throw new InvalidOperationException("Improper relink of event, this is a bug!");
        }
    }

    private void Relink(MethodDefinition method) {
        // hopefully nothing will use the ParameterDef from the MethodReturnType class...
        method.ReturnType = Relink(method.ReturnType, method);

        for (int i = 0; i < method.Parameters.Count; i++) {
            method.Parameters[i].ParameterType = Relink(method.Parameters[i].ParameterType, method);
        }
        
        for (int i = 0; i < method.Overrides.Count; i++) {
            // these can't be a GenericInstanceType according to the spec, so the
            // ctx won't really be used here
            method.Overrides[i] = Relink(method.Overrides[i], method);
        }

        for (int i = 0; i < method.GenericParameters.Count; i++) {
            GenericParameter gp = method.GenericParameters[i];
            // There's not much to do with generic parameters, other than to relink the constraints
            for (int j = 0; j < gp.Constraints.Count; j++) {
                gp.Constraints[j].ConstraintType = Relink(gp.Constraints[j].ConstraintType, method /* won't be used anyway */);
            }
        }
        
        // Relink the body
        MethodBody body = method.Body;
        for (int i = 0; i < body.Variables.Count; i++) {
            body.Variables[i].VariableType = Relink(body.Variables[i].VariableType, method);
        }

        foreach (Instruction instruction in body.Instructions) {
            switch (instruction.Operand) {
                // Reference-less types
                case string:
                case sbyte:
                case byte:
                case int:
                case long:
                case float:
                case double:
                case null:
                case Instruction:
                case Instruction[]:
                // These have been relinked already and were not replaced either in the process
                case VariableDefinition:
                case ParameterDefinition:
                    break;
                // Actual references
                case TypeReference tRef:
                    instruction.Operand = Relink(tRef, method);
                    break;
                case FieldReference fRef:
                    instruction.Operand = Relink(fRef, method);
                    break;
                case MethodReference mRef:
                    instruction.Operand = Relink(mRef, method);
                    break;
                case CallSite callSite:
                    // Ideally we relink the method reference it holds, unfortunately its private
                    CallSite newCs = new(/* ReturnType = */ Relink(callSite.ReturnType, method));
                    foreach (ParameterDefinition param in callSite.Parameters) {
                        newCs.Parameters.Add(new ParameterDefinition(param.Name, param.Attributes, Relink(param.ParameterType, method)));
                    }
                    instruction.Operand = newCs;
                    break;
                default:
                    throw new InvalidOperationException($"Unexpected operand type {instruction.Operand.GetType()}");
            }
        }
        
        RelinkCAs(method.CustomAttributes);
    }

    // For some reason you cannot read the type this interface impl is defined on
    private void Relink(InterfaceImplementation interfaceImplementation, TypeDefinition ctx) {
        interfaceImplementation.InterfaceType = Relink(interfaceImplementation.InterfaceType, ctx);
        
        RelinkCAs(interfaceImplementation.CustomAttributes);
    }
    
    #endregion
    
    #region Extras

    private void RelinkCAs(Collection<CustomAttribute> attributes) {
        if (attributes.Count == 0) return;
        foreach (CustomAttribute ca in attributes) {
            ca.Constructor = Relink(ca.Constructor, null /* There's no context here */);

            for (int i = 0; i < ca.ConstructorArguments.Count; i++) {
                // Just relink the TypeDefinitions that may appear as values since all other possible values are primitives anyway
                // The Type of the argument does not need to be relinked either since its always primitive or Type as well
                if (ca.ConstructorArguments[i].Value is not TypeDefinition tDef) continue;
                ca.ConstructorArguments[i] = new CustomAttributeArgument(ca.ConstructorArguments[i].Type, Relink(tDef, null /* No context here */));
            }

            foreach (Collection<CustomAttributeNamedArgument> col in (Span<Collection<CustomAttributeNamedArgument>>) [ca.Fields, ca.Properties]) {
                for (int i = 0; i < col.Count; i++) {
                    if (col[i].Argument.Value is not TypeDefinition tDef) continue;
                    // This is very ugly, but there are no setters on anything, so we must recreate everything
                    // And we also have to dup code because mono decided to use its special Collection<T> type for lists :/ (no ref allowed magic)
                    col[i] = new CustomAttributeNamedArgument(col[i].Name, 
                        new CustomAttributeArgument(col[i].Argument.Type, Relink(tDef, null /* No context here */)));
                }
            }
        }
    }

    private static RelinkMap<T> IdentityMap<T>() where T : MemberReference {
        return i => i;
    }

    private GenericParameter PairWithParent(GenericParameter source, IGenericParameterProvider parent) {
        if (source.Position < 0 || source.Position >= parent.GenericParameters.Count) throw new ArgumentOutOfRangeException(nameof(source));
        return parent.GenericParameters[source.Position];
    }

    // Relink the arguments, note that there can only be a single GIT in the DeclaringType tree (and it's at the end)
    // `member` is not mutated
    private TypeReference[] RelinkGenericArgsFromDeclTypes(MemberReference member, IMemberDefinition? ctx) {
        if (member.DeclaringType == null) return [];
        TypeReference[] ret = [];
        if (member.DeclaringType is GenericInstanceType git) {
            ret = new TypeReference[git.GenericArguments.Count];
            for (int i = 0; i < ret.Length; i++) {
                ret[i] = Relink(git.GenericArguments[i], ctx);
            }
        }
#if DEBUG
        TypeReference curr = member.DeclaringType;
        int amount = 0;
        while (curr != null) {
            if (curr.DeclaringType is GenericInstanceType) {
                amount++;
            }
            
            curr = curr.DeclaringType;
        }
        // The entirety of the generic arguments of all the types (including nested) will be in the topmost declaring type
        if (amount > 1) throw new NotSupportedException("Found type with multiple GenericInstanceTypes in its declaring type tree, this is not supported!");
#endif
        return ret;
    }

    // This method assumes `memberDest` is a cloned ref that can be messed with
    // Assume that GenericInstanceTypes can only appear in the DeclaringType of the MemberReferences and nowhere else in the DeclaringType tree
    private void ReAttachGenericArgumentsLike(MemberReference memberDest, TypeReference[] genericArgs) {
        if (memberDest.DeclaringType is GenericInstanceType) {
            throw new InvalidOperationException("Tried to attach generic arguments on an already generic type!");
        }
        GenericInstanceType declDestGIT = new(memberDest.DeclaringType);
        foreach (TypeReference ga in genericArgs) {
            declDestGIT.GenericArguments.Add(ga);
        }
        memberDest.DeclaringType = declDestGIT;
    }

    #endregion
}
