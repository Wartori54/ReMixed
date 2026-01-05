using System;
using System.Reflection;
using Mono.Cecil;
using ReMixed.MethodAttribute;
using ReMixed.PlatformImpls;

namespace ReMixed.Tests;

public class MergeTest {
    public static void TestMerge() {
        AssemblyDefinition asm = AssemblyDefinition.ReadAssembly(typeof(MergeTest).Assembly.Location, new ReaderParameters(ReadingMode.Immediate));
        CecilPlatform platform = new();
        MixinApply.MixinMergeAndRelink(platform, asm.MainModule);
        asm.Write(typeof(MergeTest).Assembly.Location + ".relink");
        Assembly outAsm = Assembly.LoadFile(typeof(MergeTest).Assembly.Location + ".relink");
        Type outType = outAsm.GetType("ReMixed.Tests.OrigClass")!;
        object o = Activator.CreateInstance(outType)!;
        outType.GetMethod("TestMethod")!.Invoke(o, []);
        outType.GetMethod("TestMethodInjection")!.Invoke(o, [2, true]);
    }
}

public class OrigClass {
    public int Field1;
    public string? Field2;
    public int OrigProp { get; set; }
    
    public OrigClass() {
        Console.WriteLine("Constructor");
    }

    public void TestMethod() {
        Console.WriteLine("TestMethod");
    }

    public int TestMethodInjection(int arg1, bool arg2) {
        Console.WriteLine("TestMethodInjection " + arg2);
        int doStuff = 0;
        for (int i = 0; i < 10; i++) {
            doStuff += arg1;
        }
        Console.WriteLine("Stuff done!");
        TestCall();
        if (doStuff == 10) {
            Console.WriteLine("Other return!");
            return 3;
        }
        TestCall2();
        Console.WriteLine("DoStuff: " + doStuff);
        Console.WriteLine("Stuff Done!");
        return 2;
    }

    public void TestCall() {
        
    }
    
    public void TestCall2() {}
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

    [Overwrite]
    public void TestMethod() {
        Console.WriteLine("TestOverwrite");
    }

    [Inject("TestMethodInjection", ["HEAD"])]
    public void TestMethodInjectionNoArgs(ILPatcher.CallbackInfoRet<int> ci) {
        Console.WriteLine("TestMethodInjection no args!");
    }
    
    [Inject("TestMethodInjection", ["HEAD"])]
    public void TestMethodInjectionArgs(ILPatcher.CallbackInfoRet<int> ci, int arg1, bool arg2) {
        Console.WriteLine("TestMethodInjection with args!");
        Console.WriteLine(arg1);
        Console.WriteLine(arg2);
    }

    [Inject("TestMethodInjection", ["TAIL"])]
    public static void TestMethodInjectionStaticTail(ILPatcher.CallbackInfoRet<int> ci, int arg1, bool arg2) {
        Console.WriteLine("TestMethodInjectionStaticTail with args!");
    }
    
    [Inject("TestMethodInjection", ["RETURN"])]
    public static void TestMethodInjectionStaticReturn(ILPatcher.CallbackInfoRet<int> ci, int arg1, bool arg2) {
        Console.WriteLine("TestMethodInjectionStaticReturn with args! " + arg1);
    }

    [Inject("TestMethodInjection", ["CALL:1", "CALL:2"]), AtPos("1", "System.Void ReMixed.Tests.OrigClass::TestCall()"), AtPos("2", "System.Void ReMixed.Tests.OrigClass::TestCall2()")]
    public void TestMethodInjectionCall(ILPatcher.CallbackInfoRet<int> ci, int arg1, bool arg2) {
        Console.WriteLine("TestMethodInjectionCall with args! " + arg1);
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
