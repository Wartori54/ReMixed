using System;
using System.Collections.Generic;
using Mono.Cecil;
using ReMixed.MethodAttribute;

namespace ReMixed.Injection;

public abstract class Injector {
    
    protected readonly MethodPatchContext Context;
    protected PatchPlatform Platform => Context.Platform;
    
    protected Injector(MethodPatchContext context) {
        Context = context;
    }

    protected Injector Create(InjectorID id) => Platform.InjectorRegistry.Create(id, Context);
    protected T Create<T>(InjectorID id) where T : Injector {
        Injector inj = Create(id);
        return inj as T ?? throw new InvalidOperationException($"The provided {nameof(InjectorRegistry.InjectorFactory)} for id {id} was not assignable to {typeof(T)} (actual type: {inj.GetType()}"); 
    }
    
    public abstract MethodPatchContext.Positioner GetRentSize(MethodPatchContext.Positioner positioner);

    public abstract void Inject(MethodPatchContext.Cursor cursor, IMethodSignature targetSig, MethodDefinition source);

    public sealed class InjectorRegistry {
        public delegate Injector InjectorFactory(MethodPatchContext context);
        public delegate Injector AttributeInjectorFactory(MethodPositionedAttribute attribute, MethodPatchContext context);
        
        public delegate bool PositionerAction(MethodPatchContext.Positioner positioner, AtPosAttribute? atAttr, int itr);

        private readonly Dictionary<string, InjectorFactory> registeredIds = new();

        private readonly Dictionary<Type, AttributeInjectorFactory> registeredAttrs = new();
        
        private readonly Dictionary<string, PositionerAction> registeredPositioners = new();

        public void RegisterForAttribute(Type type, AttributeInjectorFactory injFact) {
            if (!type.IsAssignableTo(typeof(MethodPositionedAttribute))) throw new ArgumentException($"{type} must inherit {typeof(MethodPositionedAttribute).FullName}", nameof(type));
            registeredAttrs[type] = injFact;
        }

        public Injector CreateForAttribute(MethodPositionedAttribute attr, MethodPatchContext methodPatchContext) {
            Type type = attr.GetType();
            if (!type.IsAssignableTo(typeof(MethodPositionedAttribute))) throw new ArgumentException($"{type} must inherit {typeof(MethodPositionedAttribute).FullName}", nameof(attr));
            return registeredAttrs.GetOrThrow(type, $"No Injector registered for attribute: {type.FullName}")(attr, methodPatchContext);
        }

        public void Register(InjectorID id, InjectorFactory injFact) {
            // Factories are overridable intentionally
            registeredIds[id.Id] = injFact;
        }

        public Injector Create(InjectorID id, MethodPatchContext methodPatchContext) {
            return registeredIds.GetOrThrow(id.Id, $"No Injector registered for id: {id}")(methodPatchContext);
        }

        public void RegisterPositioner(string id, PositionerAction positioner) {
            registeredPositioners[id] = positioner;
        }

        public PositionerAction GetPositioner(string id) {
            return registeredPositioners.GetOrThrow(id, $"No Positioner registered for id: {id}");
        }
    }
}