using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ReMixed.Gen;

// TODO: certain type names are hardcoded, find a better solution!
[Generator(LanguageNames.CSharp)]
public class MonoModMixinGlue : IIncrementalGenerator {
    private const string BaseTargetFullName = "ReMixed.Extends";
    private const string BaseTargetName = "Extends";
    private const string InjectAttributeFullName = "ReMixed.Positioning.InjectAttribute";
    private const string CallbackInfoFullName = "ReMixed.ILPatcher.CallbackInfo";
    private const string BepInExOriginalAttributesAttributeFullName =
        "BepInEx.AssemblyPublicizer.OriginalAttributesAttribute";
    
    public void Initialize(IncrementalGeneratorInitializationContext context) {
        IncrementalValuesProvider<Util.GlueSyntaxTarget> values = context.SyntaxProvider.CreateSyntaxProvider(static (node, _) => IsSyntaxTargetForGeneration(node),
                static (syntaxContext, _) => GetSemanticTargetForGeneration(syntaxContext))
            .Where(static cls => cls is not null)!;

        context.RegisterSourceOutput(values, Execute);
    }

    private static bool IsSyntaxTargetForGeneration(SyntaxNode node) {
        if (node is ClassDeclarationSyntax CDS && CDS.BaseList != null) {
            foreach (BaseTypeSyntax typeSyntax in CDS.BaseList.Types) {
                if (typeSyntax.ToString().StartsWith(BaseTargetName + "<")) {
                    return true;
                }
            }
        }

        return false;
    }

    private static Util.GlueSyntaxTarget? GetSemanticTargetForGeneration(GeneratorSyntaxContext context) {
        // Obtain the target class data
        ClassDeclarationSyntax classDeclarationSyntax = (ClassDeclarationSyntax)context.Node;
        // And its methods
        INamedTypeSymbol? classSymbol = context.SemanticModel.GetDeclaredSymbol(classDeclarationSyntax);
        if (classSymbol?.BaseType == null || classSymbol.BaseType.TypeArguments.Length != 1) return null;
        if (!classSymbol.BaseType.ToDisplayString().StartsWith(BaseTargetFullName)) return null;
        ITypeSymbol targetClassSymbol = classSymbol.BaseType.TypeArguments[0];
        List<IMethodSymbol> targetClassMethods = targetClassSymbol.GetMembers().OfType<IMethodSymbol>().ToList();
        
        // Find which methods need gluing
        List<(IMethodSymbol, IMethodSymbol)> methodTargets = [];
        foreach (MemberDeclarationSyntax member in classDeclarationSyntax.Members) {
            if (member is not MethodDeclarationSyntax method) continue;
            foreach (AttributeListSyntax attributeListSyntax in method.AttributeLists) {
                foreach (AttributeSyntax attributeSyntax in attributeListSyntax.Attributes) {
                    if (ModelExtensions.GetSymbolInfo(context.SemanticModel, attributeSyntax)
                            .Symbol is not IMethodSymbol attributeSymbol) {
                        continue;
                    }

                    if (attributeSymbol.ContainingType.ToDisplayString() == InjectAttributeFullName) {
                        // This method is an inject target, find its corresponding method
                        IMethodSymbol? methodSymbol = context.SemanticModel.GetDeclaredSymbol(method);
                        if (methodSymbol == null) continue;
                        IMethodSymbol? foundTarget = null;
                        foreach (IMethodSymbol? targetClassMethodSymbol in targetClassMethods) {
                            if (!CompareSignatures(targetClassMethodSymbol, methodSymbol)) continue;
                            foundTarget = targetClassMethodSymbol;
                            break;
                        }
                        if (foundTarget == null) continue; // No valid target, no-op TODO: warn the user
                        methodTargets.Add((methodSymbol, foundTarget));
                    }
                }
            }
        }

        return new Util.GlueSyntaxTarget(targetClassSymbol, classDeclarationSyntax, methodTargets);
    }

    // Compares two method signatures, verifying if the `sourceTarget` is in a valid hook signature for the `hookingTarget`
    private static bool CompareSignatures(IMethodSymbol hookingTarget, IMethodSymbol sourceTarget) {
        if (hookingTarget.Name != sourceTarget.Name) return false;
        if (sourceTarget.IsGenericMethod) // Hooking generics is (for now) disallowed
            return false;
        // First arg is always the CI
        if (sourceTarget.Parameters.Length != hookingTarget.Parameters.Length + 1) 
            return false;
        // Verify the CI type
        {
            ITypeSymbol? retType = null;
            INamedTypeSymbol ciArgument = (INamedTypeSymbol)sourceTarget.Parameters[0].Type;
            // Both start equally
            if (!ciArgument.ToDisplayString().StartsWith(CallbackInfoFullName)) {
                return false;
            }
            if (ciArgument.TypeArguments.Length != 0) { // its a CIR
                retType = ciArgument.TypeArguments[0];
            } // otherwise its a CI

            if (retType == null) { // Return type should be void
                if (!hookingTarget.ReturnsVoid) return false;
            } else {
                if (!hookingTarget.ReturnType.Equals(retType, SymbolEqualityComparer.IncludeNullability)) return false;
            }
        }
        for (int i = 1; i < sourceTarget.Parameters.Length; i++) {
            if (!sourceTarget.Parameters[i].Type.Equals(hookingTarget.Parameters[i-1].Type, SymbolEqualityComparer.IncludeNullability)) {
                return false;
            }
        }

        return true;
    }

    // Creates a name from the method name and parameters that should be unique to it
    private static string GetDetourMethodName(IMethodSymbol methodSymbol) {
        StringBuilder methodSig = new();
        // TODO: make this fast!
        foreach (ITypeSymbol parameter in methodSymbol.Parameters.Select(p => p.Type)) {
            methodSig.Append("___" + parameter.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)
                .Replace(" ", "")
                .Replace("?", "_nullable")
                .Replace("<", "_gs").Replace(">", "_ge")
                .Replace(",", "_"))
                .Replace("[", "_as").Replace("]", "_ae")
                .Replace("(", "_ts").Replace(")", "_te")
                .Replace("&", "_")
                .Replace("*", "_ptr");
        }
        
        return methodSymbol.Name + methodSig + "_Detour";
    }

    private static string GenerateReflectionFor(IMethodSymbol methodSymbol, Util.TypeProxy typeProxy) {
        string typeofPart = $"typeof({typeProxy.ProxyType(methodSymbol.ContainingType)})";
        string getMethodNamePart = $".GetMethod(\"{methodSymbol.Name}\", ";

        // Dirty fix for publicized assemblies, BepInEx only currently supported
        bool isPublicized = methodSymbol.GetAttributes().Any(i => i.AttributeClass?.ToDisplayString() == BepInExOriginalAttributesAttributeFullName);
        
        string getMethodFlagsPart;
        if (methodSymbol.DeclaredAccessibility == Accessibility.NotApplicable)
            getMethodFlagsPart = "";
        else
            getMethodFlagsPart = $"{(methodSymbol.DeclaredAccessibility == Accessibility.Public && !isPublicized ? "BindingFlags.Public" : "BindingFlags.NonPublic")}";
        
        getMethodFlagsPart += " | ";
        getMethodFlagsPart += $"{(methodSymbol.IsStatic ? "BindingFlags.Static" : "BindingFlags.Instance")}, ";

        string[] typeOfs = new string[methodSymbol.Parameters.Length];
        for (int i = 0; i < methodSymbol.Parameters.Length; i++) {

            typeOfs[i] = $"typeof({typeProxy.ProxyType(methodSymbol.Parameters[i].Type)})";
        }

        string getMethodTypesPart = $"new Type[] {{{string.Join(", ", typeOfs)}}})";

        return typeofPart + getMethodNamePart + getMethodFlagsPart + getMethodTypesPart;
    }

    private static void Execute(SourceProductionContext ctx, Util.GlueSyntaxTarget glueSyntaxTarget) {
        // Get the required data
        string namespaceTarget = (glueSyntaxTarget.Cds.Parent as BaseNamespaceDeclarationSyntax)?.Name.ToString() ?? throw new InvalidOperationException();
        string targetClass = glueSyntaxTarget.TypeProxy.ProxyType(glueSyntaxTarget.TargetClass);
        string workingClass = glueSyntaxTarget.Cds.Identifier.ToString();
        
        // SBs
        StringBuilder usingSb = new();
        StringBuilder sb = new();
        
        // Start the generation
        usingSb.AppendLine(Util.MonoModGlueClassTargetUsings);
        sb.AppendLine($"");
        sb.AppendLine($"namespace {namespaceTarget};");
        sb.AppendLine($"");
        sb.AppendLine($"#nullable enable"); // SG files need nullability enabled manually
        sb.AppendLine($"");
        sb.AppendLine($"public partial class {workingClass} {{");
        sb.AppendLine($"    private static bool MMGLUE_Applied;");
        sb.AppendLine($"");
        sb.AppendLine($"    public static void MMGLUE_Patch() {{");
        sb.AppendLine($"        if (MMGLUE_Applied) throw new InvalidOperationException(\"Attempted to apply twice an Extends<T>\");");
        sb.AppendLine($"        MMGLUE_Applied = true;");
        sb.AppendLine($"        ConstructorInfo[] ctors = typeof({targetClass}).GetConstructors();");
        sb.AppendLine($"        for (int i = 0; i < ctors.Length; i++) {{");
        sb.AppendLine($"            DeferredMonoModPlatform.AutoHook(ctors[i], cursor => {{");
        sb.AppendLine($"                ILPatcher.InjectCallAt(cursor, new InjectLocation(InjectTarget.FromString(\"HEAD\"), false),");
        sb.AppendLine($"                    typeof({workingClass}).GetMethod(nameof(ctorPatch), BindingFlags.NonPublic | BindingFlags.Static)!);");
        sb.AppendLine($"            }});");
        sb.AppendLine($"        }}");
        foreach ((IMethodSymbol methodSymbol, IMethodSymbol hookTargetMethodSymbol) in glueSyntaxTarget.MethodSymbols) {
            string detourMethodName = GetDetourMethodName(hookTargetMethodSymbol);
            sb.AppendLine(
                      $"        InjectAttribute {detourMethodName}_attribute = {GenerateReflectionFor(methodSymbol, glueSyntaxTarget.TypeProxy)}!.GetCustomAttribute<InjectAttribute>()!;");
            
            sb.AppendLine(
                      $"        DeferredMonoModPlatform.AutoHook({GenerateReflectionFor(hookTargetMethodSymbol, glueSyntaxTarget.TypeProxy)}!, il => {{");
            // TODO: Shift support
            sb.AppendLine(
                      $"            ILPatcher.InjectCallAt{(hookTargetMethodSymbol.ReturnsVoid ? "" : $"<{glueSyntaxTarget.TypeProxy.ProxyType(hookTargetMethodSymbol.ReturnType)}>")}(il, new InjectLocation(InjectTarget.FromString({detourMethodName}_attribute.MethodTarget), {detourMethodName}_attribute.Cancellable, {detourMethodName}_attribute.Shift, {detourMethodName}_attribute.Index), ((Delegate){(hookTargetMethodSymbol.IsStatic ? methodSymbol.Name : detourMethodName)}).Method);");
            // sb.AppendLine(
                      // $"            MonoModPlatform.LogAllInstrs(il);");
            sb.AppendLine(
                      $"        }});");
            sb.AppendLine(
                      $"");
        }
        sb.AppendLine($"    }}");
        sb.AppendLine($"");
        sb.AppendLine($"    private static void ctorPatch(ILPatcher.CallbackInfo ci, {targetClass} instance) {{");
        sb.AppendLine($"        ExtendManager.AddEntry(instance, new {workingClass}(instance));");
        sb.AppendLine($"    }}");
        foreach ((IMethodSymbol methodSymbol, IMethodSymbol hookTargetMethodSymbol) in glueSyntaxTarget.MethodSymbols) {
            if (methodSymbol.IsStatic) continue; // Static methods dont need this
            string detourMethodName = GetDetourMethodName(hookTargetMethodSymbol);
            // Generate args
            string[] argsSig = new string[hookTargetMethodSymbol.Parameters.Length];
            string[] argsCall = new string[hookTargetMethodSymbol.Parameters.Length];
            for (int i = 0; i < hookTargetMethodSymbol.Parameters.Length; i++) {
                argsSig[i] = glueSyntaxTarget.TypeProxy.ProxyType(hookTargetMethodSymbol.Parameters[i].Type) + " " + $"arg{i}";
                argsCall[i] = $"arg{i}";
            }
            string argsSigStr = string.Join(", ", argsSig);
            if (argsSigStr != "") argsSigStr = ", " + argsSigStr;
            string argsCallStr = string.Join(", ", argsCall);
            if (argsCallStr != "") argsCallStr = ", " + argsCallStr;
            
            sb.AppendLine(
                      $"");
            sb.AppendLine(
                      $"    private static void {detourMethodName}(ILPatcher.CallbackInfo{(hookTargetMethodSymbol.ReturnsVoid ? "" : $"Ret<{glueSyntaxTarget.TypeProxy.ProxyType(hookTargetMethodSymbol.ReturnType)}>")} ci, {targetClass} _this{argsSigStr}) {{");
            sb.AppendLine(
                      $"        {workingClass} target = ExtendManager.GetEntry<{workingClass}, {targetClass}>(_this, o => new {workingClass}(o));");
            sb.AppendLine(
                      $"        target.{methodSymbol.Name}(ci{argsCallStr});");
            sb.AppendLine(
                      $"    }}");
        }
        sb.AppendLine($"}}");

        usingSb.AppendLine();
        glueSyntaxTarget.TypeProxy.GenerateUsings(usingSb);
        
        ctx.AddSource($"{workingClass}.MonoModMixinGlue.g.cs", usingSb.ToString() + sb.ToString());
    }

}