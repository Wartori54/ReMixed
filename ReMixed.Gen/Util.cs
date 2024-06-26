using System.Collections.Generic;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace ReMixed.Gen;

public static class Util {
    public const string MonoModGlueClassTargetUsings =
        """
        // <auto-generated />
        
        using ReMixed.PlatformImpls;
        using ReMixed.Positioning;
        using ReMixed;
        
        using MonoMod.Cil;
        using MonoMod.RuntimeDetour;
        
        using System;
        using System.Reflection;
        using System.Collections.Generic;
        """;
    
    public class GlueSyntaxTarget(ITypeSymbol targetClass, ClassDeclarationSyntax cds, List<(IMethodSymbol, IMethodSymbol)> methodSymbols) {
        public ITypeSymbol TargetClass { get; } = targetClass;
        public ClassDeclarationSyntax Cds { get; } = cds;
        public List<(IMethodSymbol, IMethodSymbol)> MethodSymbols { get; } = methodSymbols;
        public TypeProxy TypeProxy = new();
    }
    
    public class TypeProxy {
        private const string ProxiedTypePfx = "PT";
        private Dictionary<string, string> ProxiedTypes = new();

        public string ProxyType(ITypeSymbol type) {
            if (ProxiedTypes.TryGetValue(type.ToDisplayString(), out string value)) {
                return value;
            }

            string pType = ProxiedTypePfx + ProxiedTypes.Count;
            ProxiedTypes[type.ToDisplayString()] = pType;
            return pType;
        }

        public void GenerateUsings(StringBuilder sb) {
            foreach (KeyValuePair<string, string> pair in ProxiedTypes) {
                sb.AppendLine($"using {pair.Value} = {pair.Key};");
            }
        }

        public void Clear() {
            ProxiedTypes.Clear();
        }
    }
}