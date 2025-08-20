#define VERIFY_ARGS

using System;
using Mono.Cecil;
using Mono.Cecil.Cil;
using ReMixed.MethodAttribute;

namespace ReMixed.Injection;

public sealed class MethodCallInjector : Injector {
    private readonly CIInjector ciInjector;
    private readonly Injector instanceInjector;

    public static InjectorRegistry.InjectorFactory Factory => m => new MethodCallInjector(m);

    public static InjectorRegistry.AttributeInjectorFactory AttributeFactory => (a, m) => {
        InjectAttribute injectAttribute = a as InjectAttribute ?? throw new Exception($"Cannot create {nameof(MethodCallInjector)} with an attribute of type {a.GetType()}");
        if (!injectAttribute.Cancellable)
            return new MethodCallInjector(m);
        else
            throw new NotImplementedException();
    };

    private MethodCallInjector(MethodPatchContext context) : base(context) {
        ciInjector = Create<CIInjector>(InjectorIds.CIInjectorNonCancellable);
        instanceInjector = Create(InjectorIds.InstanceInjector);
    }

    public override MethodPatchContext.Positioner GetRentSize(MethodPatchContext.Positioner positioner) {
        // No space required
        return positioner;
    }
    public override void Inject(MethodPatchContext.Cursor cursor, IMethodSignature targetSig, MethodDefinition injectMethod) {
        // Do analysis
        (bool shouldEmitInstance, int capturedArgs) = AnalyzeMethodReference(injectMethod, targetSig);
        // Figure out instance, before anything
        if (shouldEmitInstance) {
            EmitInstance(cursor, targetSig, injectMethod);
        }
        
        // Push the CI
        ciInjector.Inject(cursor, targetSig, injectMethod);

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

    private void EmitInstance(MethodPatchContext.Cursor cursor, IMethodSignature targetSig, MethodDefinition injectMethod) {
        instanceInjector.Inject(cursor, targetSig, injectMethod);
    }
    
     // Verifies an injection method contains the correct arguments for its task
    private (bool shouldEmitInstance, int capturedArgs) AnalyzeMethodReference<T1, T2>(T1 injectedMethod, 
        T2 destination) where T1 : IMethodSignature, IGenericParameterProvider where T2 : IMethodSignature {
        if (injectedMethod.HasGenericParameters) { // TODO
            throw new NotImplementedException();
        }
        
        // Allow instance -> instance, static -> static and static -> instance
        if (injectedMethod.HasThis && !destination.HasThis) {
            throw new Exception("Malformed injection method: Tried to inject a non-static method into a static one!");
        }
        bool shouldEmitInstance = injectedMethod.HasThis;
        
        if (injectedMethod.Parameters.Count == 0) throw new Exception("Malformed injection method: Must take a CallbackInfo or CallbackInfoReturnable as first arg!"); // TODO: Doc errors
        
        // Java mixins have the CI and CIR at the end of the argument list, TODO: should this be changed?
        // Must start with a callback info of some type
        if (!ILPatcher.TypeReferenceEqual(injectedMethod.Parameters[0].ParameterType, Platform.ThisCecilDefs.CIReference) && 
            !ILPatcher.TypeReferenceEqual(injectedMethod.Parameters[0].ParameterType, Platform.ThisCecilDefs.CIRReferenceM(destination.ReturnType)))
            throw new Exception("Malformed injection method: Must take a CallbackInfo or CallbackInfoReturnable as first arg!");
        
        // Can be a ci alone
        if (injectedMethod.Parameters.Count == 1) // Captures no arguments
            return (shouldEmitInstance, 0);
        
        if (injectedMethod.Parameters.Count != destination.Parameters.Count + 1 /* CI or CIR */)
            throw new Exception("Malformed injection method: Must take the same amount of arguments as well as a CallbackInfo or CallbackInfoReturnable as first arg!");

        // Check next args
        int arg;
        for (arg = 0; arg < destination.Parameters.Count; arg++) {
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
    
    // TODO: Add ability to capture return value
    protected CIInjector(MethodPatchContext context, bool cancel) : base(context) {
        cancellable = cancel;
    }

    public static InjectorRegistry.InjectorFactory FactoryNonCancellable => m => new CIInjector(m, false);
    public static InjectorRegistry.InjectorFactory FactoryCancellable => m => new CIInjector(m, true);

    public override MethodPatchContext.Positioner GetRentSize(MethodPatchContext.Positioner positioner) {
        return positioner;
    }
    public override void Inject(MethodPatchContext.Cursor cursor, IMethodSignature targetSig, MethodDefinition source) {
        // Verify the parameter
        if (targetSig.ReturnType.FullName != "System.Void") { // This void check is kinda ugly
            if (!ILPatcher.TypeReferenceEqual(source.Parameters[0].ParameterType, 
                    Platform.ThisCecilDefs.CIRReferenceM(targetSig.ReturnType))) {
                throw new Exception($"Malformed injection method: Non-void injection must take a CallbackInfoRet<{targetSig.ReturnType}> as first arg");
            }
        } else {
            if (!ILPatcher.TypeReferenceEqual(source.Parameters[0].ParameterType,
                    Platform.ThisCecilDefs.CIReference)) {
                throw new Exception($"Malformed injection method: Void injection must take a CallbackInfo as first arg");
            }
        }
        
        // Inject:
        // ldstr "methodname"
        // (ldc.i4.1/0) -- present if ret value is not void; 0 means non-cancellable, 1 means cancellable
        // newobj CallbackInfo/CallbackInfoReturnable -- returnable when ret value is not void
        MethodReference ctor;
        cursor.EmitLdstr("TODO: Not implemented yet" /* targetSig.Name */); // TODO
        cursor.EmitLdcI4(cancellable ? 1 : 0);
        if (targetSig.ReturnType != source.Module.TypeSystem.Void) {
            ctor = Platform.ThisCecilDefs.CIRCtorT(targetSig.ReturnType);
        } else {
            ctor = Platform.ThisCecilDefs.CICtor;
        }
        cursor.EmitNewobj(Context.Method.Module.ImportReference(ctor));
    }
}

public class SimpleInstanceInjector : Injector {
    public static InjectorRegistry.InjectorFactory Factory => m => new SimpleInstanceInjector(m);
    protected SimpleInstanceInjector(MethodPatchContext context) : base(context) {
    }

    public override MethodPatchContext.Positioner GetRentSize(MethodPatchContext.Positioner positioner) {
        return positioner;
    }
    public override void Inject(MethodPatchContext.Cursor cursor, IMethodSignature targetSig, MethodDefinition source) {
        cursor.EmitLdarg0();
    }
}

public sealed class OverwriteInjector : Injector {
    public static InjectorRegistry.AttributeInjectorFactory AttributeFactory => (attr, m) => {
        if (attr.At.Length != 1 || attr.At[0] != "HEAD") throw new InvalidOperationException("Overwrite requires position to be a single HEAD!");
        return new OverwriteInjector(m);
    };
    
    public OverwriteInjector(MethodPatchContext context) : base(context) {
    }
    
    public override MethodPatchContext.Positioner GetRentSize(MethodPatchContext.Positioner positioner) {
        if (positioner.Index != 0) throw new InvalidOperationException($"{nameof(OverwriteInjector)} must be positioned at the top!");
        // Overwrite englobes everything
        positioner.GotoLast();
        return positioner;
    }
    
    public override void Inject(MethodPatchContext.Cursor cursor, IMethodSignature targetSig, MethodDefinition source) {
        EnsureSameSignature(targetSig, source);
        
        cursor.RemoveAll();

        cursor.ClearLocals();

        foreach (VariableDefinition variable in source.Body.Variables) {
            cursor.CreateLocal(variable.VariableType);
        }
        
        foreach (Instruction instruction in source.Body.Instructions) {
            cursor.Emit(instruction.Clone());
        }
    }

    private static void EnsureSameSignature(IMethodSignature targetSig, IMethodSignature source) {
        if (targetSig.Parameters.Count != source.Parameters.Count) throw new InvalidOperationException("Signatures must be identical (different parameter count)");
        for (int i = 0; i < targetSig.Parameters.Count; i++) {
            if (!ILPatcher.TypeReferenceEqual(targetSig.Parameters[i].ParameterType, source.Parameters[i].ParameterType)) {
                throw new InvalidOperationException($"Signatures must be identical (different {i}-th parameter)");
            }
        }
        if (targetSig.ReturnType != source.ReturnType) throw new InvalidOperationException("Signatures must be identical (different return type)");
        if (targetSig.HasThis != source.HasThis) throw new InvalidOperationException("Signatures must be identical (different HasThis)");
        if (targetSig.ExplicitThis != source.ExplicitThis) throw new InvalidOperationException("Signatures must be identical (different ExplicitThis)");
        if (targetSig.CallingConvention != source.CallingConvention) throw new InvalidOperationException("Signatures must be identical (different CallingConvention)");
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
