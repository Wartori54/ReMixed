using System;
using Mono.Cecil;
using ReMixed.PlatformImpls;

namespace ReMixed.Tests;

public class MergeTest {
    public static void TestMerge() {
        AssemblyDefinition asm = AssemblyDefinition.ReadAssembly(typeof(MergeTest).Assembly.Location, new ReaderParameters(ReadingMode.Immediate));
        CecilPlatform platform = new();
        MixinApply.MixinMergeAndRelink(platform, asm.MainModule);
        asm.Write(typeof(MergeTest).Assembly.Location + ".relink");
    }
}

public class OrigClass {
    public int Field1;
    public string? Field2;
    
    public OrigClass() {
        Console.WriteLine("Constructor");
    }
}

[Mixin(typeof(OrigClass))]
public class MixinClass {
    public int NewField;
    private string? NewString;

    public void CrazyMethod() {
        MixinClass i = new();
        i.NewField = 1;
        Console.WriteLine("CrazyMethod");
        Console.WriteLine(NewField);
        Console.WriteLine(i.NewString);
        OrigClass j = new();
        j.Field1 = 2;
        Console.WriteLine(j.Field1);
        Console.WriteLine(((OrigClass) (object)this).Field1);
    }
}
