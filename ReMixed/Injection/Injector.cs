using System;
using System.Collections.Generic;
using System.Linq;
using Mono.Cecil;
using ReMixed.MethodAttribute;
using ReMixed.Registry;

namespace ReMixed.Injection;

public abstract class Injector {
    
    // private static Dictionary<Type, Type>? registeredTypes;
    //
    // private static Dictionary<Type, Type> RegisteredTypes {
    //     get {
    //         if (registeredTypes != null) return registeredTypes;
    //         registeredTypes = new Dictionary<Type, Type>();
    //
    //         foreach (Type[] types in AppDomain.CurrentDomain.GetAssemblies()
    //                      .Select(a => a.GetTypes())) {
    //             foreach (Type type in types) {
    //                 object[] attrs = type.GetCustomAttributes(typeof(InjectorMPAAttribute), true);
    //                 if (attrs.Length == 0) continue;
    //                 foreach (InjectorMPAAttribute injectorMpaAttribute in attrs) {
    //                     if (registeredTypes.TryGetValue(injectorMpaAttribute.MPAType, out Type? conflictType)) {
    //                         throw new Exception($"MPA type conflict with type {type.FullName} and {conflictType.FullName} for MPA with type: {injectorMpaAttribute.MPAType.FullName}");
    //                     }
    //                     registeredTypes[injectorMpaAttribute.MPAType] = type;
    //                 }
    //             }
    //         }
    //         
    //         return registeredTypes;
    //     }
    // }
    
    // public static Injector FromAttribute(MethodPositionedAttribute attribute, MethodPatchContext context) {
    //     if (!RegisteredTypes.TryGetValue(attribute.GetType(), out Type? injectorType)) {
    //         throw new Exception($"Unregistered injector for {attribute.GetHashCode()}!");
    //     }
    //
    //     object? instance = Activator.CreateInstance(injectorType, [context]);
    //     if (instance == null) throw new Exception($"Could not create instance of injector: {injectorType.FullName}");
    //
    //     return (Injector) instance;
    // }

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

    public abstract void Inject(MethodPatchContext.Cursor cursor, PatchableMethodDefinition patchableMethodDefinition, MethodReference source);

    public sealed class InjectorRegistry {
        public delegate Injector InjectorFactory(MethodPatchContext context);
        public delegate Injector AttributeInjectorFactory(MethodTargetAttribute attribute, MethodPatchContext context);

        private readonly Dictionary<string, InjectorFactory> registeredIds = new();

        private readonly Dictionary<Type, AttributeInjectorFactory> registeredAttrs = new();

        public void RegisterForAttribute(Type type, AttributeInjectorFactory injFact) {
            if (!type.IsAssignableTo(typeof(MethodTargetAttribute))) throw new ArgumentException($"{type} must inherit {typeof(MethodTargetAttribute).FullName}", nameof(type));
            registeredAttrs[type] = injFact;
        }

        public Injector CreateForAttribute(MethodTargetAttribute attr, MethodPatchContext methodPatchContext) {
            Type type = attr.GetType();
            if (!type.IsAssignableTo(typeof(MethodTargetAttribute))) throw new ArgumentException($"{type} must inherit {typeof(MethodTargetAttribute).FullName}", nameof(attr));
            return registeredAttrs.GetOrThrow(type, $"No Injector registered for attribute: {type.FullName}")(attr, methodPatchContext);
        }

        public void Register(InjectorID id, InjectorFactory injFact) {
            // Factories are overridable intentionally
            registeredIds[id.id] = injFact;
        }

        public Injector Create(InjectorID id, MethodPatchContext methodPatchContext) {
            return registeredIds.GetOrThrow(id.id, $"No Injector registered for id: {id}")(methodPatchContext);
        }
    }
}