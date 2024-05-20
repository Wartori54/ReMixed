using System;
using System.Collections.Generic;
using System.IO;
using Mono.Cecil;
using Mono.Cecil.Cil;
using MonoMod.Cil;
using MonoMod.RuntimeDetour;
using ReMixed.PlatformImpls;
using ReMixed.Positioning;

namespace ReMixed.Tests;

public class InjectTest {
    public static bool firstCall = true;

    public static void Main() {
        TestInject();
    }

    public static void TestInject() {
        
        
        // ILHook ilHook = MonoModPlatform.Hook(typeof(InjectTest).GetMethod(nameof(TargetMethod))!, ILModify);
        // ilHook.Apply();
        //
        // int a = TargetMethod();
        //
        // Console.WriteLine("TM: " + a);
        // ilHook.Dispose();

        // ILHook ilHook2 = MonoModPlatform.Hook(typeof(InjectTest).GetMethod(nameof(OtherTargetMethod))!, ILModify2);
        // ilHook2.Apply();
        //
        // OtherTargetMethod();
        // OtherTargetMethod();
        //
        // ilHook2.Dispose();

        // TestClass tClass = new();
        // ILHook ilHook3 = MonoModPlatform.Hook(((Delegate)tClass.ArgTestMethod).Method, ILModify3);
        
        // tClass.ArgTestMethod(1, "dos", i => i + "0");
        
        // ilHook3.Dispose();

        // ILHook ilHook4 = MonoModPlatform.Hook(typeof(InjectTest).GetMethod(nameof(SwitchTargetMethod))!, ILModify4);
        // ilHook4.Apply();
        //
        // SwitchTargetMethod(4);
        //
        // ilHook4.Dispose();
        
        TargetClassExtension.MMGLUE_Patch();
        DeferredMonoModPlatform.ApplyAll();
        TargetClass tClass = new(null);
        Console.WriteLine(tClass.Add());
        Console.WriteLine(tClass.Inc(false));
        Console.WriteLine(tClass.Inc(true));
        Console.WriteLine(tClass.Inc(true));
        Console.WriteLine(tClass.Inc(true));
        Console.WriteLine(tClass.Inc(true));
        Console.WriteLine(tClass.Inc(true));
        Console.WriteLine(tClass.Inc(true));
        TargetClass.PrintSomething("the thing");
        TargetClass.PrintSomething("kx");
        tClass.AddOne([1, 2, 3]).ForEach(Console.WriteLine);
        
        // int b = Console.Read();
        // int c = Console.Read();
        // int d = Console.Read();
        // Console.WriteLine(b);
        // if (b > 100) {
        //     d++;
        //     if (c < 3) {
        //         d++;
        //         c = 0;
        //     } else {
        //         b *= b;
        //     }
        // } else {
        //     if (d < 10)
        //         c = 5;
        //     if (c > 5) {
        //         b++;
        //         d++;
        //         if (d < 10) d = 10;
        //     } else {
        //         b = c = d = 5;
        //     }
        // }
        // Console.WriteLine(b + " " + c + " " + d);
        // double e = (2.5*d) + 2.4;
    }

    public static void ILModify(PatchPlatform.Cursor cursor) {
        ILPatcher.ReplaceNextConst(cursor, 0, 7);
        ILPatcher.ReplaceNextConst(cursor, 10, 5);
        ILPatcher.InjectCallAt<int>(cursor, 
            new InjectLocation(InjectTarget.FromString("System.Console; void WriteLine(string?)"),
                true,
                Idx: 1,
                ShiftBy: InjectLocation.Shift.BeforeArguments),
            ((Delegate)ILModify_Inj1).Method);
        ILPatcher.InjectCallAt<int>(cursor,
            new InjectLocation(InjectTarget.FromString("TAIL"), true),
            ((Delegate)ILModify_Inj2).Method);
        // cursor.GotoFirst();
        // ILPatcher.EmitCallbackInfoRet(cursor);
        // cursor.Emit(OpCodes.Pop, null);
        
        MonoModPlatform.LogAllInstrs((ILCursor)cursor._RealType);
        // cir.Cancel();
    }

    public static void ILModify_Inj1(ILPatcher.CallbackInfoRet<int> cir) {
        Console.WriteLine("before call :eyes:");
        // if (firstCall) {
            // cir.Cancel(0);
            // firstCall = false;
        // }
    }

    public static void ILModify_Inj2(ILPatcher.CallbackInfoRet<int> cir) {
        cir.Cancel(-100);
    }

    public static void ILModify2(PatchPlatform.Cursor cursor) {
        ILPatcher.InjectCallAt(cursor, new InjectLocation(InjectTarget.FromString("ReMixed.Tests.InjectTest; object AmazingTargetMethod(ReMixed.Tests.InjectTest.TestDelegate, System.Func<int, System.List<int>>, string)"), 
                false, InjectLocation.Shift.BeforeArguments),
            ((Delegate)ILModify2_Inj1).Method);
        ILPatcher.InjectCallAt(cursor, new InjectLocation(InjectTarget.FromString("TAIL"), true),
            ((Delegate)ILModify2_Inj2).Method);
        MonoModPlatform.LogAllInstrs((ILCursor)cursor._RealType);
    }
    
    public static void ILModify2_Inj1(ILPatcher.CallbackInfo ci) {
        Console.WriteLine("BEFORE THE THINGGG");
        // ci.Cancel();
    }

    public static void ILModify2_Inj2(ILPatcher.CallbackInfo ci) {
        Console.WriteLine("TAIL");
        if (firstCall) {
            firstCall = false;
            return;
        }

        ci.Cancel();
    }

    public static void ILModify3(PatchPlatform.Cursor cursor) {
        TestClass t = new();
        MethodReference mRef = ((ILCursor)cursor._RealType).Context.Module.ImportReference(((Delegate)t.ArgTestMethod).Method);
        MethodDefinition mDef = mRef.Resolve();
        Console.WriteLine(mDef.IsStatic);
        ILPatcher.InjectCallAt(cursor, new InjectLocation(InjectTarget.FromString("HEAD"), false), 
            ((Delegate) ILModify3_Inj).Method);
        MonoModPlatform.LogAllInstrs((ILCursor)cursor._RealType);
    }

    public static void ILModify3_Inj(ILPatcher.CallbackInfo ci, TestClass _this, int a, string b, Func<int, string> c) {
        Console.WriteLine("Pre running the func: " + c(a));
        Console.WriteLine(a % 2 == 0 ? "a is even" : "a is odd");
        Console.WriteLine("Secret string is: " + _this.theString);
    }
    
    public static int TargetMethod() {
        Console.WriteLine("FIRST CALL!");
        int num = 0;
        Console.WriteLine("num: " + num);
        num++;
        Console.WriteLine("num: " + num);
        num = 10;
        // ILPatcher.CallbackInfoRet<float> cir = new();
        // cir.IsCanceled();
        // cir.SetReturnValue(5.3f);
        // cir.Cancel();
        // cir.GetRet();
        Console.WriteLine("BEFORE RETURN!");
        return num;
    }

    public static void OtherTargetMethod() {
        Console.WriteLine("Prior thing");
        AmazingTargetMethod(
        s => {
                Console.WriteLine(s);
                return s.Length;
            }, 
             GetARRLambda(),
            "abc"
        );
        Console.WriteLine("Final thing");
    }

    public static Func<int, List<int>> GetARRLambda() {
        Console.WriteLine("Obtaining lambda");
        return s => {
            Console.WriteLine("INT TIME!");
            return [s];
        };
    }

    public delegate int TestDelegate(string a);

    public static object AmazingTargetMethod(TestDelegate t, Func<int, List<int>> f, string s) {
        return f.Invoke(t.Invoke(s));
    }

    public static void ILModify4(PatchPlatform.Cursor cursor) {
        ILPatcher.InjectCallAt(cursor, new InjectLocation(InjectTarget.FromString("RETURN"), true), ((Delegate)ILModify4_Inj1).Method);
        ILPatcher.InjectCallAt(cursor, new InjectLocation(InjectTarget.FromString("RETURN"), true), ((Delegate)ILModify4_Inj1).Method);
        MonoModPlatform.LogAllInstrs(cursor);
    }

    public static void ILModify4_Inj1(ILPatcher.CallbackInfo ci, int a) {
        Console.WriteLine("BEF RET!");
        Console.WriteLine(a);
    }
    
    public static void SwitchTargetMethod(int a) {
        switch (a) {
            case 1:
                Console.WriteLine("a");
                break;
            case 3:
            case 2:
                Console.WriteLine("b");
                break;
            case 4:
                Console.WriteLine("d");
                return;
            case 142:
            case 152:
                Console.WriteLine("e");
                break;
            default:
                Console.WriteLine("unknown");
                return;
        }
    }

    public class TestClass {
        public string theString = "THESTRING";

        public void ArgTestMethod(int a, string b, Func<int, string> c) {
            Console.WriteLine(a);
            Console.WriteLine(b);
            Console.WriteLine(c(a));
            Console.WriteLine(theString);
        }
    }

    public static int TestCIRMethod() {
        List<int> a = [2];
        ILPatcher.CallbackInfoRet<int> cir = new();
        {
            cir.Cancel(2);
        }
        // if (cir.IsCanceled())
        //     return cir.GetRet();
        Console.WriteLine(a[0]);
        return 0;
    }
}