using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Mono.Cecil;
using Mono.Cecil.Cil;

namespace ReMixed;

public static class Extensions {
    /// <summary>Matches an instruction with the given opcode</summary>
    /// <param name="instr">The instruction to try to match.</param>
    /// <param name="opcode">The instruction opcode to match.</param>
    /// <returns><see langword="true"/> if the instruction matches; <see langword="false"/> otherwise.</returns>
    public static bool Match(this Instruction instr, OpCode opcode) {
        if (opcode == null) throw new ArgumentNullException(nameof(opcode));
        return instr.OpCode == opcode;
    }

    /// <summary>Matches an instruction with the given opcode</summary>
    /// <param name="instr">The instruction to try to match.</param>
    /// <param name="opcode">The instruction opcode to match.</param>
    /// <param name="value">The operand value of the instruction.</param>
    /// <returns><see langword="true"/> if the instruction matches; <see langword="false"/> otherwise.</returns>
    public static bool Match<T>(this Instruction instr, OpCode opcode, T value)
        => instr.Match(opcode, out T? v) && (v?.Equals(value) ?? value == null);

    /// <summary>Matches an instruction with the given opcode</summary>
    /// <param name="instr">The instruction to try to match.</param>
    /// <param name="opcode">The instruction opcode to match.</param>
    /// <param name="value">The operand value of the instruction.</param>
    /// <returns><see langword="true"/> if the instruction matches; <see langword="false"/> otherwise.</returns>
    public static bool Match<T>(this Instruction instr, OpCode opcode, [MaybeNullWhen(false)] out T value) {
        if (instr == null) throw new ArgumentNullException(nameof(instr));
        if (instr.OpCode == opcode) {
            value = (T)instr.Operand;
            return true;
        } else {
            value = default;
            return false;
        }
    }

    /// <summary>Matches an instruction with opcode <see cref="OpCodes.Ldarg"/>.</summary>
    /// <param name="instr">The instruction to try to match.</param>
    /// <param name="value">The operand value of the instruction.</param>
    /// <returns><see langword="true"/> if the instruction matches; <see langword="false"/> otherwise.</returns>
    public static bool MatchLdarg(this Instruction instr, out int value) {
        if (instr == null) throw new ArgumentNullException(nameof(instr));
        if (instr.OpCode == OpCodes.Ldarg || instr.OpCode == OpCodes.Ldarg_S) {
            value = ((ParameterReference)instr.Operand).Index;
            return true;
        } else if (instr.OpCode == OpCodes.Ldarg_0) {
            value = 0;
            return true;
        } else if (instr.OpCode == OpCodes.Ldarg_1) {
            value = 1;
            return true;
        } else if (instr.OpCode == OpCodes.Ldarg_2) {
            value = 2;
            return true;
        } else if (instr.OpCode == OpCodes.Ldarg_3) {
            value = 3;
            return true;
        } else {
            value = default;
            return false;
        }
    }

    /// <summary>Matches an instruction with opcode <see cref="OpCodes.Starg"/>.</summary>
    /// <param name="instr">The instruction to try to match.</param>
    /// <param name="value">The operand value of the instruction.</param>
    /// <returns><see langword="true"/> if the instruction matches; <see langword="false"/> otherwise.</returns>
    public static bool MatchStarg(this Instruction instr, out int value) {
        if (instr == null) throw new ArgumentNullException(nameof(instr));
        if (instr.OpCode == OpCodes.Starg || instr.OpCode == OpCodes.Starg_S) {
            value = ((ParameterReference)instr.Operand).Index;
            return true;
        } else {
            value = default;
            return false;
        }
    }

    /// <summary>Matches an instruction with opcode <see cref="OpCodes.Ldarga"/>.</summary>
    /// <param name="instr">The instruction to try to match.</param>
    /// <param name="value">The operand value of the instruction.</param>
    /// <returns><see langword="true"/> if the instruction matches; <see langword="false"/> otherwise.</returns>
    public static bool MatchLdarga(this Instruction instr, out int value) {
        if (instr == null) throw new ArgumentNullException(nameof(instr));
        if (instr.OpCode == OpCodes.Ldarga || instr.OpCode == OpCodes.Ldarga_S) {
            value = ((ParameterReference)instr.Operand).Index;
            return true;
        } else {
            value = default;
            return false;
        }
    }

    /// <summary>Matches an instruction with opcode <see cref="OpCodes.Ldloc"/>.</summary>
    /// <param name="instr">The instruction to try to match.</param>
    /// <param name="value">The operand value of the instruction.</param>
    /// <returns><see langword="true"/> if the instruction matches; <see langword="false"/> otherwise.</returns>
    public static bool MatchLdloc(this Instruction instr, out int value) {
        if (instr == null) throw new ArgumentNullException(nameof(instr));
        if (instr.OpCode == OpCodes.Ldloc || instr.OpCode == OpCodes.Ldloc_S) {
            value = ((VariableReference)instr.Operand).Index;
            return true;
        } else if (instr.OpCode == OpCodes.Ldloc_0) {
            value = 0;
            return true;
        } else if (instr.OpCode == OpCodes.Ldloc_1) {
            value = 1;
            return true;
        } else if (instr.OpCode == OpCodes.Ldloc_2) {
            value = 2;
            return true;
        } else if (instr.OpCode == OpCodes.Ldloc_3) {
            value = 3;
            return true;
        } else {
            value = default;
            return false;
        }
    }

    /// <summary>Matches an instruction with opcode <see cref="OpCodes.Stloc"/>.</summary>
    /// <param name="instr">The instruction to try to match.</param>
    /// <param name="value">The operand value of the instruction.</param>
    /// <returns><see langword="true"/> if the instruction matches; <see langword="false"/> otherwise.</returns>
    public static bool MatchStloc(this Instruction instr, out int value) {
        if (instr == null) throw new ArgumentNullException(nameof(instr));
        if (instr.OpCode == OpCodes.Stloc || instr.OpCode == OpCodes.Stloc_S) {
            value = ((VariableReference)instr.Operand).Index;
            return true;
        } else if (instr.OpCode == OpCodes.Stloc_0) {
            value = 0;
            return true;
        } else if (instr.OpCode == OpCodes.Stloc_1) {
            value = 1;
            return true;
        } else if (instr.OpCode == OpCodes.Stloc_2) {
            value = 2;
            return true;
        } else if (instr.OpCode == OpCodes.Stloc_3) {
            value = 3;
            return true;
        } else {
            value = default;
            return false;
        }
    }

    /// <summary>Matches an instruction with opcode <see cref="OpCodes.Ldloca"/>.</summary>
    /// <param name="instr">The instruction to try to match.</param>
    /// <param name="value">The operand value of the instruction.</param>
    /// <returns><see langword="true"/> if the instruction matches; <see langword="false"/> otherwise.</returns>
    public static bool MatchLdloca(this Instruction instr, out int value) {
        if (instr == null) throw new ArgumentNullException(nameof(instr));
        if (instr.OpCode == OpCodes.Ldloca || instr.OpCode == OpCodes.Ldloca_S) {
            value = ((VariableReference)instr.Operand).Index;
            return true;
        } else {
            value = default;
            return false;
        }
    }

    /// <summary>Matches an instruction with opcode <see cref="OpCodes.Ldc_I4"/>.</summary>
    /// <param name="instr">The instruction to try to match.</param>
    /// <param name="value">The operand value of the instruction.</param>
    /// <returns><see langword="true"/> if the instruction matches; <see langword="false"/> otherwise.</returns>
    public static bool MatchLdcI4(this Instruction instr, out int value) {
        if (instr == null) throw new ArgumentNullException(nameof(instr));
        if (instr.OpCode == OpCodes.Ldc_I4) {
            value = (int)instr.Operand;
            return true;
        } else if (instr.OpCode == OpCodes.Ldc_I4_S) {
            value = (sbyte)instr.Operand;
            return true;
        } else if (instr.OpCode == OpCodes.Ldc_I4_0) {
            value = 0;
            return true;
        } else if (instr.OpCode == OpCodes.Ldc_I4_1) {
            value = 1;
            return true;
        } else if (instr.OpCode == OpCodes.Ldc_I4_2) {
            value = 2;
            return true;
        } else if (instr.OpCode == OpCodes.Ldc_I4_3) {
            value = 3;
            return true;
        } else if (instr.OpCode == OpCodes.Ldc_I4_4) {
            value = 4;
            return true;
        } else if (instr.OpCode == OpCodes.Ldc_I4_5) {
            value = 5;
            return true;
        } else if (instr.OpCode == OpCodes.Ldc_I4_6) {
            value = 6;
            return true;
        } else if (instr.OpCode == OpCodes.Ldc_I4_7) {
            value = 7;
            return true;
        } else if (instr.OpCode == OpCodes.Ldc_I4_8) {
            value = 8;
            return true;
        } else if (instr.OpCode == OpCodes.Ldc_I4_M1) {
            value = -1;
            return true;
        } else {
            value = default;
            return false;
        }
    }

    public static bool MatchLdcI4(this Instruction instr, int value)
        => MatchLdcI4(instr, out int v) && v == value;

    public static bool MatchLdcI8(this Instruction instr, long value) {
        return instr.OpCode == OpCodes.Ldc_I8 && (long) instr.Operand == value;
    }
    
    // ReSharper disable once CompareOfFloatsByEqualityOperator
    public static bool MatchLdcR4(this Instruction instr, float value) {
        return instr.OpCode == OpCodes.Ldc_R4 && (float) instr.Operand == value;
    }
    
    // ReSharper disable once CompareOfFloatsByEqualityOperator
    public static bool MatchLdcR8(this Instruction instr, double value) {
        return instr.OpCode == OpCodes.Ldc_R8 && (double) instr.Operand == value;
    }

    /// <summary>Matches an instruction with opcode <see cref="OpCodes.Call"/> or <see cref="OpCodes.Callvirt"/>.</summary>
    /// <param name="instr">The instruction to try to match.</param>
    /// <param name="value">The operand value of the instruction.</param>
    /// <returns><see langword="true"/> if the instruction matches; <see langword="false"/> otherwise.</returns>
    public static bool MatchCallOrCallvirt(this Instruction instr, [MaybeNullWhen(false)] out MethodReference value) {
        if (instr == null) throw new ArgumentNullException(nameof(instr));
        if (instr.OpCode == OpCodes.Call || instr.OpCode == OpCodes.Callvirt) {
            value = (MethodReference)instr.Operand;
            return true;
        } else {
            value = default;
            return false;
        }
    }

    // public static bool MatchCall(this Instruction instr, Type type, string name, out MethodReference? match) {
    //     match = null;
    //     if (!MatchCallOrCallvirt(instr, out MethodReference? methodReference)) {
    //         return false;
    //     }
    //
    //     
    //     if (methodReference.DeclaringType.Is(type) && methodReference.Name == name) {
    //         match = methodReference;
    //         return true;
    //     }
    //     return false;
    // }
    
    public static bool MatchCall(this Instruction instr, string type, string name, List<string> methodParams, out MethodReference? match) {
        match = null;
        if (!MatchCallOrCallvirt(instr, out MethodReference? methodReference))
            return false;
        
#if DEBUG
        Console.WriteLine($"Checking with method: {methodReference.DeclaringType.FullName} {methodReference.Name}");
#endif
        
        if (methodReference.DeclaringType.FullName != type || methodReference.Name != name)
            return false;

        if (methodReference.Parameters.Count != methodParams.Count)
            return false;
        
        for (int i = 0; i < methodReference.Parameters.Count; i++) {
            ParameterDefinition? param = methodReference.Parameters[i];
            #if DEBUG
            Console.WriteLine(param.ParameterType.FullName);
            #endif
            if (param.ParameterType.FullName != methodParams[i]) 
                return false;
        }

        match = methodReference;
        return true;
    }

    /// <summary>
    /// Matches a call that fulfills the provided `LocalFunctionStatementSyntax`, and its declaring type.
    /// </summary>
    /// <param name="instr"></param>
    /// <param name="target"></param>
    /// <param name="type"></param>
    /// <param name="match"></param>
    /// <returns></returns>
    /// <exception cref="NotSupportedException"></exception>
    public static bool MatchCall(this Instruction instr, LocalFunctionStatementSyntax target, string type, out MethodReference? match) {
        match = null;
        if (!MatchCallOrCallvirt(instr, out match)) {
            return false;
        }

        if (match.Name != target.Identifier.ToString()) return false;
        if (match.DeclaringType.FullName != type) return false;
        
        // Check for generics now, verifying the return type may be expensive
        TypeParameterListSyntax? genericParamSyntax =
                    (TypeParameterListSyntax?)target.ChildNodes().FirstOrDefault(c => c is TypeParameterListSyntax, null);
        if (match.HasGenericParameters != (genericParamSyntax != null)) return false;
        if (match.HasGenericParameters) {
            if (match.GenericParameters.Count != genericParamSyntax!.Parameters.Count) return false;
        }
        
        if (!match.ReturnType.CompareWithSyntax(target.ReturnType)) return false;
        
        int i = 0;
        foreach (ParameterSyntax parameterSyntax in target.ParameterList.Parameters) {
            if (match.Parameters.Count <= i) return false;
            if (parameterSyntax.Type == null) throw new NotSupportedException("Cannot have type less parameters!");
            if (!match.Parameters[i].ParameterType.CompareWithSyntax(parameterSyntax.Type)) return false;
            i++;
        }

        // match.GenericParameters.Count; // TODO
        // Well maybe not, do we check if generics are properly set up?

        return true;
    }

    public static bool MatchRet(this Instruction instr) => instr.OpCode == OpCodes.Ret;

    public static int GetStackConsumeCount(this IMethodSignature sig) {
        return sig.Parameters.Count + (sig.HasThis && !sig.ExplicitThis ? 1 : 0);
    }

    /// <summary>
    /// Compares a cecil `TypeReference` to a CodeAnalysis `TypeSyntax`
    /// </summary>
    /// <param name="type">The cecil `TypeReference` to check.</param>
    /// <param name="syntax">The CodeAnaysis `TypeSyntax` to check.</param>
    /// <returns>Whether the type and the syntax are compatible.</returns>
    /// <exception cref="NotImplementedException">In case you hit an edge case.</exception>
    public static bool CompareWithSyntax(this TypeReference type, TypeSyntax syntax) {
        // Array, function pointer, pointer and byreference are mutually exclusive when matching the top node.
        // As such all of this do an early true return (with a recursive call before it)
        
        // Check for array
        if (type.IsArray != syntax is ArrayTypeSyntax) return false;
        if (type.IsArray) {
            ArrayTypeSyntax arraySyntax = (ArrayTypeSyntax)syntax;
            ArrayType arrayType = (ArrayType)type;
            if (arrayType.Rank != arraySyntax.RankSpecifiers.Count) return false;
            if (!CompareWithSyntax(arrayType.ElementType, arraySyntax.ElementType)) return false;
            return true;
        }

        // Check for function pointer
        if (type.IsFunctionPointer != syntax is FunctionPointerTypeSyntax) return false;
        if (type.IsFunctionPointer) {
            throw new NotImplementedException();
        }

        // Check for pointer
        if (type.IsPointer != syntax is PointerTypeSyntax) return false;
        if (type.IsPointer) {
            PointerType pointerType = (PointerType)type;
            PointerTypeSyntax pointerTypeSyntax = (PointerTypeSyntax)syntax;
            if (!CompareWithSyntax(pointerType.ElementType, pointerTypeSyntax.ElementType)) return false;
            return true;
        }

        // Check for byreference
        // Note: the syntax may itself be the correct type, or its parent may have a token which is a `ref` keyword, from my testing.
        if (type.IsByReference != (syntax is RefTypeSyntax ||
                                   syntax.Parent?.ChildTokens()
                                       .Any(t => t.IsKind(SyntaxKind.RefKeyword)) == true))
            return false;
        if (type.IsByReference) {
            ByReferenceType referenceType = (ByReferenceType)type;
            TypeSyntax s;
            if (syntax is RefTypeSyntax rs) s = rs.Type;
            else s = syntax; // syntax was already a non ref type, it only had ref keyword attached to it
            if (!CompareWithSyntax(referenceType.ElementType, s)) return false;
            return true;
        }

        // Nullability is an edge case
        if (syntax is NullableTypeSyntax nullableTypeSyntax) {
            // Only nullable info is preserved to IL for valuetypes
            if (type.IsValueType) {
                if (!type.HasGenericParameters || type.DeclaringType + "." + type.Name + "<>" != typeof(Nullable<>).FullName) return false;
                // Compare inner
                return CompareWithSyntax(type.GenericParameters[0], nullableTypeSyntax.ElementType);
            } else { // No info to check anyways, just unbox it
                return CompareWithSyntax(type, nullableTypeSyntax.ElementType);
            }
        }
        
        // Check for Tuple, convert it to generic if present, and then check for generics
        if (type.HasGenericParameters) { 
            IEnumerable<TypeSyntax> genericParamsSyntax;
            // Convert tuple to generic parameter list
            if (syntax is TupleTypeSyntax tupleTypeSyntax) {
                genericParamsSyntax = tupleTypeSyntax.Elements.Select(s => s.Type);
            } else if (syntax is GenericNameSyntax genericNameSyntax) {
                genericParamsSyntax = genericNameSyntax.TypeArgumentList.Arguments;
            } else {
                return false;
            }

            int i = 0;
            foreach (TypeSyntax genericParam in genericParamsSyntax) {
                if (type.GenericParameters.Count <= i) return false;
                if (!CompareWithSyntax(type.GenericParameters[i], genericParam)) return false;
                i++;
            }
        }

        // Verify the fully qualified name
        if (syntax is QualifiedNameSyntax qualifiedNameSyntax) {
            // Class declaration and such may not have a declaring type, fallback to namespace in that case
            if ((type.DeclaringType != null ? type.DeclaringType.FullName : type.Namespace) != qualifiedNameSyntax.Left.ToString()) return false;
            
            GenericNameSyntax? genericNameSyntax = qualifiedNameSyntax.Right as GenericNameSyntax;
            GenericInstanceType? genericType = type as GenericInstanceType;
            // If one is, the other must as well
            if ((genericNameSyntax == null) != (genericType == null)) return false;
            if (genericNameSyntax != null) {
                // genericType cant be null here, just think about it
                if (Typing.GetGenericSafeTypeName(genericType!) != genericNameSyntax.Identifier.ToString())
                    return false;
            } else {
                // This has to be an identifier name, it cant be anything else
                if (qualifiedNameSyntax.Right is IdentifierNameSyntax identifierNameSyntax &&
                    type.Name != identifierNameSyntax.ToString())
                    return false;
            }
        } else if (syntax is AliasQualifiedNameSyntax aliasQualifiedNameSyntax) {
            throw new NotImplementedException();
        } else if (syntax is PredefinedTypeSyntax predefinedTypeSyntax) {
            // Surely CodeAnalysis has a way a better way to do this...
            if (type.FullName != Typing.FullTypeBasicTypes(predefinedTypeSyntax.Keyword.ToString())) return false;
        } else {
            return false; // We only work with fully qualified names
        }

        

        return true;
    }

    public static bool InstrEquals(this Instruction self, Instruction other, StackAnalysis ctx, MethodBody selfBody, MethodBody otherBody) {
        if (self.OpCode != other.OpCode) return false;
        // TODO: Better operand equality
        object selfOp = self.Operand;
        object otherOp = other.Operand;
        if (ctx.OperandConverter != null) {
            selfOp = ctx.OperandConverter(self.Operand, selfBody, selfBody.Instructions);
            otherOp = ctx.OperandConverter(other.Operand, otherBody, otherBody.Instructions);
        }
        
        if (selfOp is Instruction selfInstr
            && otherOp is Instruction otherInstr) {
            return InstrEquals(selfInstr, otherInstr, ctx, selfBody, otherBody);
        }

        if (selfOp == null && other.Operand == null) return true;
        if (selfOp == null || other.Operand == null) return false;
        return self.Operand.Equals(other.Operand);
    }
}