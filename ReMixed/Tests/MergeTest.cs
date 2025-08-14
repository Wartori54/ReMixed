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
    public int OrigProp { get; set; }
    
    public OrigClass() {
        Console.WriteLine("Constructor");
    }
}

[Mixin(typeof(OrigClass))]
public class MixinClass {
    public int NewField;
    private string? NewString;
    public int Field1;

    public MixinClass otherI;

    public double PropTest {
        get => 69;
        set => throw new NotImplementedException();
    }

    public required object PropTest2 {
        get;
        init;
    }

    public void CrazyMethod() {
        MixinClass i = new() {
            NewField = 1,
            PropTest2 = null
        };
        Field1 = 7;
        Console.WriteLine("CrazyMethod");
        Console.WriteLine(NewField);
        Console.WriteLine(i.NewString);
        OrigClass j = new();
        j.Field1 = 2;
        MixinClass k = new() {
            PropTest2 = 8
        };
        k.Field1 = 3;
        Console.WriteLine(j.Field1);
        Console.WriteLine(((OrigClass) (object)this).Field1);
    }
}

public class OrigGeneric<T> {
    public T Field1;

    public T? Method() {
        return Field1;
    }

    public T GenericMethod<T1>(T a, T1 b) where T1 : new() {
        Console.WriteLine(b);
        T1 c = new();
        Console.WriteLine(c);
        Field1 = a;
        return a;
    }
}

// TODO: Test constraints relinking
[Mixin(typeof(OrigGeneric<>))]
public class MixinGeneric<T> {
    public T FieldMixin;
    public int FieldMixin2;
    public MixinClass FieldMixin3;

    public bool MethodMixin(T a) {
        FieldMixin = a;
        FieldMixin2 = 1;
        FieldMixin3 = new() {
            PropTest2 = 1
        };
        Console.WriteLine(((OrigGeneric<T>) (object) this).Field1);
        return true;
    }

    public void MethodGenericMixin<T1>(T1 b) {
        if (typeof(T) == typeof(T1)) {
            return;
        }
        Console.WriteLine(b);
    }

    public class T1<V> {
        public static Func<T, V> f;
        static T1() {
            MixinGeneric<int>.T1<float>.f = null;
        }
    }
}
