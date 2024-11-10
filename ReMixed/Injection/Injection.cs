#define VERIFY_ARGS

using System;
using Mono.Cecil;
using ReMixed.MethodAttribute;
using ReMixed.Registry;

namespace ReMixed.Injection;

[InjectorMPA(typeof(InjectAttribute))]
public class MethodCallInjector : Injector {
    private readonly CIInjector ciInjector;
    private readonly InstanceInjector instanceInjector;

    public static InjectorRegistry.InjectorFactory Factory => m => new MethodCallInjector(m);

    public static InjectorRegistry.AttributeInjectorFactory AttributeFactory => (a, m) => {
        InjectAttribute injectAttribute = a as InjectAttribute ?? throw new Exception($"Cannot create {nameof(MethodCallInjector)} with an attribute of type {a.GetType()}");
        if (!injectAttribute.Cancellable)
            return new MethodCallInjector(m);
        else
            throw new NotImplementedException();
    };
    
    protected MethodCallInjector(MethodPatchContext context) : base(context) {
        ciInjector = Create<CIInjector>(InjectorIds.CIInjectorNonCancellable);
        instanceInjector = Create<InstanceInjector>(InjectorIds.InstanceInjector);
    }

    public override void Inject(MethodPatchContext.Cursor cursor, PatchableMethodDefinition patchableMethodDefinition, MethodReference injectMethod) {
        // Do analysis
        (bool shouldEmitInstance, int capturedArgs) = AnalyzeMethodReference(injectMethod, patchableMethodDefinition);
        // Figure out instance, before anything
        if (shouldEmitInstance) {
            EmitInstance(cursor, patchableMethodDefinition, injectMethod);
        }
        
        // Push the CI
        ciInjector.Inject(cursor, patchableMethodDefinition, injectMethod);

        // And emit args, notice that EmitLdarg works differently
        for (int i = 0; i < capturedArgs; i++) {
            cursor.EmitLdarg(i);
        }

        // Finally emit the target, should it check for call instead of callvirt?
        if (!shouldEmitInstance)
            cursor.EmitCall(injectMethod);
        else
            cursor.EmitCallvirt(injectMethod);
    }

    protected virtual void EmitInstance(MethodPatchContext.Cursor cursor, PatchableMethodDefinition patchableMethodDefinition, MethodReference injectMethod) {
        cursor.EmitLdarg0();
    }
    
     // Verifies an injection method contains the correct arguments for its task
    private (bool shouldEmitInstance, int capturedArgs) AnalyzeMethodReference<T1, T2>(T1 injectedMethod, 
        T2 destination) where T1 : IMethodSignature, IGenericParameterProvider where T2 : IMemberDefinition, IMethodSignature, IGenericParameterProvider {
        if (injectedMethod.HasGenericParameters) { // TODO
            throw new NotImplementedException();
        }

        if (injectedMethod.HasThis != destination.HasThis) throw new Exception("Malformed injection method: HasThis must match between injection and target!");
        bool shouldEmitInstance = injectedMethod.HasThis;
        
        if (injectedMethod.Parameters.Count == 0) throw new Exception("Malformed injection method: Must take a CallbackInfo or CallbackInfoReturnable as first arg!"); // TODO: Doc errors
        
        // Must start with a callback info of some type
        if (!ILPatcher.TypeReferenceEqual(injectedMethod.Parameters[0].ParameterType, Platform.ThisCecilDefs.CIReference) && 
            !ILPatcher.TypeReferenceEqual(injectedMethod.Parameters[0].ParameterType, Platform.ThisCecilDefs.CIRReferenceM(destination.ReturnType)))
            throw new Exception("Malformed injection method: Must take a CallbackInfo or CallbackInfoReturnable as first arg!");
        
        // Can be a ci alone
        if (injectedMethod.Parameters.Count == 1) // Captures no arguments
            return (shouldEmitInstance, 0);

        // Check next args
        int arg;
        for (arg = 0; arg < destination.Parameters.Count; arg++) {
            if (arg + 1 >= injectedMethod.Parameters.Count) {
                break;
            }
#if VERIFY_ARGS
            TypeReference paramTypeInj = injectedMethod.Parameters[arg+1].ParameterType;
            TypeReference paramTypeDest = destination.Parameters[arg].ParameterType;
            if (!ILPatcher.TypeReferenceEqual(paramTypeInj, paramTypeDest)) {
                throw new InvalidOperationException(); // TODO: Error for this
            }
#endif
        }

        return (shouldEmitInstance, arg);
    }
    
}

public class CIInjector : Injector {
    private readonly bool cancellable;
    
    protected CIInjector(MethodPatchContext context, bool cancel) : base(context) {
        cancellable = cancel;
    }

    public static InjectorRegistry.InjectorFactory FactoryNonCancellable => m => new CIInjector(m, false);
    public static InjectorRegistry.InjectorFactory FactoryCancellable => m => new CIInjector(m, true);
    
    public override void Inject(MethodPatchContext.Cursor cursor, PatchableMethodDefinition patchableMethodDefinition, MethodReference source) {
        // Verify the parameter
        if (patchableMethodDefinition.ReturnType != patchableMethodDefinition.Module.TypeSystem.Void) {
            if (!ILPatcher.TypeReferenceEqual(source.Parameters[source.HasThis ? 1 : 0].ParameterType, 
                    Platform.ThisCecilDefs.CIRReferenceM(patchableMethodDefinition.ReturnType))) {
                throw new Exception($"Malformed injection method: Non-void injection must take a CallbackInfoRet<{patchableMethodDefinition.ReturnType}> as first arg");
            }
        } else {
            if (!ILPatcher.TypeReferenceEqual(source.Parameters[source.HasThis ? 1 : 0].ParameterType,
                    Platform.ThisCecilDefs.CIReference)) {
                throw new Exception($"Malformed injection method: Void injection must take a CallbackInfo as first arg");
            }
        }
        
        // Inject:
        // ldstr "methodname"
        // (ldc.i4.1/0) -- present if ret value is not void; 0 means non-cancellable, 1 means cancellable
        // newobj CallbackInfo/CallbackInfoReturnable -- returnable when ret value is not void
        MethodReference ctor;
        cursor.EmitLdstr(patchableMethodDefinition.Name);
        cursor.EmitLdcI4(cancellable ? 1 : 0);
        if (patchableMethodDefinition.ReturnType != source.Module.TypeSystem.Void) {
            ctor = Platform.ThisCecilDefs.CIRCtorT(patchableMethodDefinition.ReturnType);
        } else {
            ctor = Platform.ThisCecilDefs.CICtor;
        }
        cursor.EmitNewobj(ctor);
    }
}

public class InstanceInjector : Injector {

    public static InjectorRegistry.InjectorFactory Factory => m => new InstanceInjector(m);
    protected InstanceInjector(MethodPatchContext context) : base(context) {
    }
    
    public override void Inject(MethodPatchContext.Cursor cursor, PatchableMethodDefinition patchableMethodDefinition, MethodReference source) {
        cursor.EmitLdarg0();
    }
}

// public class AbsolutePositionedInjection : AttributeTargetedMethodBodyTransformer {
//     protected override bool MultiTarget { get; }
//     private readonly Predicate<Instruction> injectPredicate;
//     private bool clearsRetValue;
//
//     private AbsolutePositionedInjection(MethodPatchContext context, Predicate<Instruction> predicate, bool multiMatch, bool clearsReturn) : base(context, null!) {
//         injectPredicate = predicate;
//         MultiTarget = multiMatch;
//         clearsRetValue = clearsReturn;
//     }
//
//     // protected override bool Validate(MethodBody memberDef) {
//     //     return true;
//     // }
//
//     protected override bool SeekTarget(MethodPatchContext.Positioner positioner) {
//         if (!positioner.TryGotoNext(injectPredicate)) {
//             throw new InvalidOperationException(); // TODO: actual errors
//         }
//
//         return true;
//     }
//
//     // public override int HandleShift(IPatchContext.Cursor cursor, InjectLocation.Shift shift) {
//     //     if (shift != 0)
//     //         throw new NotSupportedException("Cannot use non-default Shift with an absolute inject target!");
//     //     // No-op, shifting is not supported by these types
//     //     // It may need to pop a single element in case the injection is near returns
//     //     return clearsRetValue && cursor.Method.ReturnType.FullName != typeof(void).FullName ? 1 : 0;
//     // }
//
//     // Represents an injection at the very first instruction of the method
//     public static AbsolutePositionedInjection HEAD(MethodPatchContext ctx) => new(ctx, _ => true, false, false);
//     // Represents an injection at the very last instruction of the method
//     public static AbsolutePositionedInjection TAIL(MethodPatchContext ctx) => new(ctx, i => i.Next == null, false, true);
//     // Represents an injection right before each return call of the method
//     public static AbsolutePositionedInjection RETURN(MethodPatchContext ctx) => new(ctx, Extensions.MatchRet, true, true);
//
//     public static Func<MethodPatchContext, AbsolutePositionedInjection>? FromString(string s) {
//         return s switch {
//             "HEAD" => HEAD,
//             "TAIL" => TAIL,
//             "RETURN" => RETURN,
//             _ => null
//         };
//     }
//     protected override void PerformMethod(MethodPatchContext.Cursor cursor, PatchableMethodDefinition patchableMethodDefinition, MethodReference sourceMethod, CustomAttribute attribute) {
//         throw new NotImplementedException();
//     }
// }
