using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using Mono.Cecil;
using ReMixed.Positioning;

namespace ReMixed;

public class ThisCecilDefs {
    public interface IThisCecilDefsProvider {
        public ThisCecilDefs Get();
    }

    public readonly ModuleDefinition ThisModule;

    public readonly TypeDefinition CIReference;
    private readonly TypeDefinition CIRReference;
    public TypeReference CIRReferenceT<T>() {
        return BuildGenericTypeInstance(CIRReference, typeof(T));
    }
    public TypeReference CIRReferenceM(TypeReference arg) {
        return BuildGenericTypeInstance(CIRReference, arg);
    }
    // public static readonly TypeDefinition CIRReference = MMReflectionImporter.ProviderNoDefault.GetReflectionImporter(ThisModule).ImportReference(typeof(ILPatcher.CallbackInfoRet<>), null).Resolve();

    public readonly MethodDefinition CICtor; // There should only be one ctor
    private readonly MethodDefinition CIRCtor;

    public readonly MethodDefinition CIIsCanceled;
    private MethodDefinition CIRGetRet;
    private MethodDefinition CIRSetReturnValue;

    public MethodReference CIRCtorT<T>() {
        return CIRCtorT(typeof(T));
    }

    public MethodReference CIRCtorT(Type type) {
        return BuildGenericTypeMethod(CIRCtor, type);
    }

    public MethodReference CIRCtorT(TypeReference type) {
        return BuildGenericTypeMethod(CIRCtor, type);
    }

    public MethodReference CIRGetRetT<T>() {
        return BuildGenericTypeMethod(CIRGetRet, typeof(T));
    }
    
    public MethodReference CIRSetReturnValueT<T>() {
        return BuildGenericTypeMethod(CIRSetReturnValue, typeof(T));
    }

    public readonly TypeReference MixinAttribute;
    public readonly TypeReference AtAttribute;
    
    public ThisCecilDefs(ModuleDefinition moduleDefinition) {
        ThisModule = moduleDefinition;
        CIReference = ThisModule.GetType(typeof(ILPatcher.CallbackInfo));
        CIRReference = ThisModule.GetType(typeof(ILPatcher.CallbackInfoRet<>));
        CICtor = CIReference.Methods.First(IsCtor);
        CIRCtor = CIRReference.Methods.First(IsCtor);
        CIIsCanceled = CIReference.Methods.First(m => m.Name == nameof(ILPatcher.CallbackInfo.IsCanceled));
        CIRGetRet = CIRReference.Methods.First(m => m.Name == nameof(ILPatcher.CallbackInfoRet<int>.GetRet)); // int is used as a place holder here
        CIRSetReturnValue = CIRReference.Methods.First(m => m.Name == nameof(ILPatcher.CallbackInfoRet<int>.SetReturnValue));
        AtAttribute = ThisModule.GetType(typeof(AtAttribute));
        MixinAttribute = ThisModule.GetType(typeof(MixinAttribute));
    }
    
    private readonly Dictionary<Type, TypeReference> reflCache = new();

    public TypeReference GetReflection(Type type) {
        if (reflCache.TryGetValue(type, out TypeReference? value)) return value;
        return reflCache[type] = ThisModule.GetType(type);
    }

    private GenericInstanceType BuildGenericTypeInstance(TypeReference tref, Type type) => BuildGenericTypeInstance(tref, tref.Module.ImportReference(type));

    private GenericInstanceType BuildGenericTypeInstance(TypeReference tref, TypeReference type) {
        GenericInstanceType gi = new(tref);
        gi.GenericArguments.Add(type);
        return gi;
    }

    private MethodReference BuildGenericTypeMethod(MethodDefinition mref, Type type) {
        return mref.AttachToGIT(BuildGenericTypeInstance(mref.DeclaringType, type));
    }

    private MethodReference BuildGenericTypeMethod(MethodDefinition mref, TypeReference type) {
        return mref.AttachToGIT(BuildGenericTypeInstance(mref.DeclaringType, type));
    }
    private static bool IsCtor(MethodReference mref) => mref.Name == ".ctor";
}

public class DefaultThisCecilDefsProvider : ThisCecilDefs.IThisCecilDefsProvider {
    public static readonly string ThisModulePath = Assembly.GetExecutingAssembly().Location;
    public ThisCecilDefs Get() {
        return new ThisCecilDefs(ModuleDefinition.ReadModule(ThisModulePath));
    }
}