using System;
using System.Collections.Generic;
using ReMixed.Positioning;

namespace ReMixed.Tests;

public partial class TargetClassExtension : Extends<TargetClass> {

    public int c = 5;
    
    public TargetClassExtension(TargetClass obj) : base(obj) {
    }

    [Inject("TAIL", true)]
    public void Add(ILPatcher.CallbackInfoRet<int> cir) {
        cir.Cancel(_this.a + _this.b + this.c);
    }

    [Inject("HEAD", true)]
    public void Inc(ILPatcher.CallbackInfoRet<int> cir, bool t) {
        if (t && _this.a > _this.b) {
            Console.WriteLine("DOING STUFF!!");
            cir.Cancel(0);
        }
    }

    [Inject("System.Console; void WriteLine(string?)", true, index: 0, shift: InjectLocation.Shift.BeforeArguments)]
    public static void PrintSomething(ILPatcher.CallbackInfo cir, string arg) {
        if (arg.Length > 3)
            cir.Cancel();
    }

    [Inject("TAIL", true)]
    public void SecretInc(ILPatcher.CallbackInfoRet<int> cir) {
        c++;
        cir.Cancel(_this.a + _this.b + this.c);
    }

    [Inject("HEAD")]
    public static void SecretMethod(ILPatcher.CallbackInfo ci, List<int> c) {
        Console.WriteLine("Secret method heh " + c.Count);
    }

    [Inject("HEAD", true)]
    public void AddOne(ILPatcher.CallbackInfoRet<List<int>> cir, List<int> arg) {
        arg.Add(1);
    }
    
}
