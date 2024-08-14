using System;
using Mono.Cecil;
using Mono.Cecil.Cil;

namespace ReMixed.Transformer;

public class AbsolutePositionedInjection : MethodBodyTransformer {
    protected override bool MultiTarget { get; }
    private readonly Predicate<Instruction> injectPredicate;
    private bool clearsRetValue;

    private AbsolutePositionedInjection(MethodPatchContext context, Predicate<Instruction> predicate, bool multiMatch, bool clearsReturn) : base(context) {
        injectPredicate = predicate;
        MultiTarget = multiMatch;
        clearsRetValue = clearsReturn;
    }

    // protected override bool Validate(MethodBody memberDef) {
    //     return true;
    // }

    public override bool SeekTarget(MethodPatchContext.Positioner positioner) {
        if (!positioner.TryGotoNext(injectPredicate)) {
            throw new InvalidOperationException(); // TODO: actual errors
        }

        return true;
    }

    public override void PerformMethod(MethodPatchContext.Cursor cursor, PatchableMethodDefinition patchableMethodDefinition, MethodReference sourceMethod) {
        
    }

    // public override int HandleShift(IPatchContext.Cursor cursor, InjectLocation.Shift shift) {
    //     if (shift != 0)
    //         throw new NotSupportedException("Cannot use non-default Shift with an absolute inject target!");
    //     // No-op, shifting is not supported by these types
    //     // It may need to pop a single element in case the injection is near returns
    //     return clearsRetValue && cursor.Method.ReturnType.FullName != typeof(void).FullName ? 1 : 0;
    // }

    // Represents an injection at the very first instruction of the method
    public static AbsolutePositionedInjection HEAD(MethodPatchContext ctx) => new(ctx, _ => true, false, false);
    // Represents an injection at the very last instruction of the method
    public static AbsolutePositionedInjection TAIL(MethodPatchContext ctx) => new(ctx, i => i.Next == null, false, true);
    // Represents an injection right before each return call of the method
    public static AbsolutePositionedInjection RETURN(MethodPatchContext ctx) => new(ctx, Extensions.MatchRet, true, true);

    public static Func<MethodPatchContext, AbsolutePositionedInjection>? FromString(string s) {
        return s switch {
            "HEAD" => HEAD,
            "TAIL" => TAIL,
            "RETURN" => RETURN,
            _ => null
        };
    }
}