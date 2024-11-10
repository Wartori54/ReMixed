using System;
using System.Reflection;
using Mono.Cecil;
using ReMixed.Injection;
using ReMixed.MethodAttribute;

namespace ReMixed;

public abstract class PatchPlatform : IDisposable {

    // TODO: Allow multiple simultaneous platforms?
    //public static PatchPlatform? Instance { get; private set; }
    public string Name { get; }

    public ThisCecilDefs ThisCecilDefs { get; }

    public ILPatcher ILPatcher { get; }
    
    public abstract PatchableMethodDefinition.IMethodPool PatchableMethodPool { get; }
    
    public Injector.InjectorRegistry InjectorRegistry { get; }

    protected PatchPlatform(string name, ThisCecilDefs.IThisCecilDefsProvider thisCecilDefsProvider) {
        ThisCecilDefs = thisCecilDefsProvider.Get();
        Name = name;
        ILPatcher = new ILPatcher(ThisCecilDefs);
        InjectorRegistry = new Injector.InjectorRegistry();
        // if (Instance != null) throw new InvalidOperationException();
        // Instance = this;
    }

    public virtual void Register() {
        InjectorRegistry.RegisterForAttribute(typeof(InjectAttribute), MethodCallInjector.AttributeFactory);
        
        InjectorRegistry.Register(InjectorIds.CIInjectorNonCancellable, CIInjector.FactoryNonCancellable);
        InjectorRegistry.Register(InjectorIds.CIInjectorCancellable, CIInjector.FactoryCancellable);
        InjectorRegistry.Register(InjectorIds.InstanceInjector, InstanceInjector.Factory);
    }

    public virtual void Dispose() {
        // Instance = null;
    }
}