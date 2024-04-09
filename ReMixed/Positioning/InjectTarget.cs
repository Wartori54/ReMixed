using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Mono.Cecil;
using Mono.Cecil.Cil;

namespace ReMixed.Positioning;

/// <summary>
/// Represents an inject location
/// </summary>
public abstract class InjectTarget {
    /// <summary>
    /// Whether this InjectTarget may inject the same payload into multiple places.
    /// Effectively stops/forces to continue the patcher routine from/to keep checking other instructions.
    /// </summary>
    public abstract bool MultiMatch { get; }
    /// <summary>
    /// Returns whether the current instruction is a valid inject location.
    /// </summary>
    /// <param name="instruction">The instruction to test.</param>
    /// <returns>Whether its the target InjectTarget.</returns>
    public abstract bool Predicate(Instruction instruction);

    /// <summary>
    /// Does the required setup to get a cursor in the correct location according to the shift value.
    /// </summary>
    /// <param name="cursor">The cursor to move.</param>
    /// <param name="shift">The current shift setting</param>
    /// <returns>The amount of elements that may need to be popped when canceling.</returns>
    public abstract int HandleShift(PatchPlatform.Cursor cursor, InjectLocation.Shift shift);

    /// <summary>
    /// Instantiates a new InjectTarget from a string.
    /// </summary>
    /// <param name="s"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException">If the string is not valid for any InjectTarget type.</exception>
    public static InjectTarget FromString(string s) {
        InjectTarget? ret;
        ret = AbsolutePositionedInjectTarget.FromString(s); // It is not that
        if (ret != null) return ret;
        ret = MethodCallInjectTarget.FromString(s);
        if (ret == null) throw new NotSupportedException(); // Uh oh
        return ret;
    }
}

/// <summary>
/// Represents an absolute InjectTarget, that is, an InjectTarget is relative to the method, not its content.
/// </summary>
public class AbsolutePositionedInjectTarget : InjectTarget {
    public override bool MultiMatch { get; }
    private readonly Predicate<Instruction> injectPredicate;
    private bool clearsRetValue;

    private AbsolutePositionedInjectTarget(Predicate<Instruction> predicate, bool multiMatch, bool clearsReturn) {
        injectPredicate = predicate;
        MultiMatch = multiMatch;
        clearsRetValue = clearsReturn;
    }

    public override bool Predicate(Instruction instruction) => injectPredicate(instruction);
    
    public override int HandleShift(PatchPlatform.Cursor cursor, InjectLocation.Shift shift) {
        if (shift != 0)
            throw new NotSupportedException("Cannot use non-default Shift with an absolute inject target!");
        // No-op, shifting is not supported by these types
        // It may needed to pop a single element in case the injection is near returns
        return clearsRetValue && cursor.Method.ReturnType.FullName != typeof(void).FullName ? 1 : 0;
    }

    // Represents an injection at the very first instruction of the method
    public static readonly AbsolutePositionedInjectTarget HEAD = new(i => true, false, false);
    // Represents an injection at the very last instruction of the method
    public static readonly AbsolutePositionedInjectTarget TAIL = new(i => i.Next == null, false, true);
    // Represents an injection right before each return call of the method
    public static readonly AbsolutePositionedInjectTarget RETURN = new(Extensions.MatchRet, true, true);

    public new static AbsolutePositionedInjectTarget? FromString(string s) {
        return s switch {
            "HEAD" => HEAD,
            "TAIL" => TAIL,
            "RETURN" => RETURN,
            _ => null
        };
    }
}

/// <summary>
/// Represents an InjectTarget before/after a certain method.
/// </summary>
public class MethodCallInjectTarget : InjectTarget {
    public override bool MultiMatch => false; // Injects to a single place by definition
    private readonly string type;
    private readonly LocalFunctionStatementSyntax functionSyntax;
    private MethodReference? methodReference = null;
    public MethodReference? MethodReference => methodReference;

    private MethodCallInjectTarget(string target) {
        string[] parts = target.Split(';');
        if (parts.Length != 2)
            throw new NotSupportedException(
                "Parameter must contain exactly 1 semicolon between the type and the method decl");
        type = parts[0];
        
        //https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/types
        SyntaxTree tree = CSharpSyntaxTree.ParseText(parts[1]);
        LocalFunctionStatementSyntax? localFunctionStatementSyntax = tree
            .GetRoot() // SyntaxNode
            .ChildNodes() // Childs
            .First() // First will be a `GlobalStatementSyntax`
            .ChildNodes() // Childs of that
            .First() as LocalFunctionStatementSyntax; // Must be this type, otherwise the method is not in a valid form
        functionSyntax = localFunctionStatementSyntax ?? throw new NotSupportedException("Cannot parse method target!");
    }
    
    public override bool Predicate(Instruction instruction) {
        return instruction.MatchCall(functionSyntax, type, out methodReference);
    }

    public override int HandleShift(PatchPlatform.Cursor cursor, InjectLocation.Shift shift) {
        // Remember to translate the modified instr indexes to orig indexes
        int callInstr = cursor.Platform.InjectionTracker.CalculateOrigIndex(cursor.Method.Body.Instructions.IndexOf(cursor.Next));
        if (callInstr == -1) throw new InvalidOperationException("Cannot obtain instruction index!");
        StackAnalysis stackAnalysis = cursor.Platform.StAnalysis;
        IMethodSignature mr = (IMethodSignature)cursor.Next!.Operand; // Next must be a call
        StackAnalysis.StackFrame startFrame = stackAnalysis.StackFrames[callInstr];
        switch (shift) {
            case InjectLocation.Shift.After when MethodReference == null:
                throw new InvalidOperationException(); // Method must be resolved at this point
            // easier case
            case InjectLocation.Shift.After: {
                int movIdx = 1;
                cursor.GotoNext();
                // TODO: Consider this
                if (MethodReference.ReturnType.FullName != typeof(void).FullName) { // try to move after any assignments
                    // Dont go crazy, if next instr the stack goes down by one go for it
                    // MAYBETODO?
                } // If its void then the value cannot be stored and thus theres no need to do literally anything

                StackAnalysis.StackFrame targetFrame = stackAnalysis.StackFrames[callInstr+movIdx];
                return targetFrame.stackAmount;
            }
            // Move RIGHT BEFORE the call instruction is executed, after all arguments are on the stack
            case InjectLocation.Shift.Before: {
                // Actually we are already there, so only calculate pop values
                return startFrame.stackAmount;
            }
            // Move before all arguments are pushed to the stack for the method
            case InjectLocation.Shift.BeforeArguments: {
                int stackElements = startFrame.stackAmount;
                int extraElements = stackElements -
                                    (mr.GetStackConsumeCount() -
                                     (cursor.Next.OpCode.Code == Code.Newobj
                                         ? -1
                                         : 0)); // TODO: HACKFIX, newobj is marked as HasThis but does not consume a This
                int currFrame = callInstr;
                while (stackAnalysis.StackFrames[currFrame].stackAmount > extraElements) {
                    // If while going back we hit a branch target, it is required for argument creation to start at 
                    // least at the branch instruction, as such, it is required to move over there, even if the stack 
                    // is the required level at any point inside the branch
                    if (stackAnalysis.Branches.TryGetValue(currFrame, out List<int>? jmpStart)) {
                        currFrame = jmpStart.Min(); // The earliest is guaranteed to work
                    }
                    currFrame--;
                    if (currFrame < 0) throw new InvalidOperationException();
                }

                cursor.MoveIndex(cursor.Platform.InjectionTracker.CalculateModifiedIndex(currFrame - callInstr));
                return extraElements;
            }
            default:
                throw new ArgumentOutOfRangeException(nameof(shift));
        }
    }

    public new static MethodCallInjectTarget FromString(string s) {
        return new MethodCallInjectTarget(s);
    }
}
