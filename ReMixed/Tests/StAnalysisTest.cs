using System;
using System.Linq;
using Mono.Cecil;

namespace ReMixed.Tests;

public class StAnalysisTest {
    public static void TestStAnalysis() {
        AssemblyDefinition asm = AssemblyDefinition.ReadAssembly("ReMixed.dll");
        TypeDefinition thisType = asm.MainModule.GetType(typeof(StAnalysisTest).FullName);

        StackAnalysis stackAnalysis = new(thisType.Methods.First(m => m.Name == nameof(NotSoSimple)));
        foreach (StackAnalysis.StackFrame stackFrame in stackAnalysis.StackFrames) {
            for (int i = 0; i < stackFrame.stackAmount; i++) {
                Console.Write("=");
            }
            Console.WriteLine();
        }
        
        TypeDefinition otherType = asm.MainModule.GetType(typeof(StackAnalysis).FullName);
        StackAnalysis stackAnalysis2 = new(otherType.Methods.First(m => m.Name == "GetStackRes"));
        foreach (StackAnalysis.StackFrame stackFrame in stackAnalysis2.StackFrames) {
            for (int i = 0; i < stackFrame.stackAmount; i++) {
                Console.Write("=");
            }
            Console.WriteLine();
        }
        
        
        AssemblyDefinition asmIL = AssemblyDefinition.ReadAssembly("ILTestMethods.dll");
        TypeDefinition ILMethods = asmIL.MainModule.GetType("ReMixed.Tests.ILClass");
        
        StackAnalysis stackAnalysis3 = new(ILMethods.Methods.First(m => m.Name == "FancyBr"));
        foreach (StackAnalysis.StackFrame stackFrame in stackAnalysis3.StackFrames) {
            for (int i = 0; i < stackFrame.stackAmount; i++) {
                Console.Write("=");
            }
            Console.WriteLine();
        }
    }

    public string SimpleMethod(int c) {
        Console.WriteLine("A b c");
        int a = 2 + c;
        a++;
        a *= a;
        a -= a;
        Console.WriteLine(a);
        string b = $"{a} a";
        return b;
    }

    public void NotSoSimple(int a) {
        while (a-- > 0) {
            Console.WriteLine(a);
        }
    }
}
