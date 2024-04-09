using System;
using System.Collections.Generic;

namespace ReMixed.Tests;

public class TargetClass {
    public int a = 2;
    public int b = 3;

    public TargetClass(int? a) {
        if (a != null) this.a = a.Value;
    }

    public void Print() {
        Console.WriteLine(a);
        Console.WriteLine(b);
    }

    public int Add() => a + b;
    public int Inc(bool t) => t ? a++ : b++;

    public int Inc(bool t, int c) => t ? a += c : b += c;

    private int SecretInc() {
        a++;
        b++;
        return a + b;
    }

    public static void PrintSomething(string a) {
        Console.WriteLine("Hello world!");
        Console.WriteLine("Received: " + a);
    }

    private static void SecretMethod(List<int> b) {
        foreach (int d in b) {
            Console.WriteLine(d);
        }
    }

    public List<int> AddOne(List<int> list) {
        for (int i = 0; i < list.Count; i++) {
            list[i]++;
        }

        return list;
    }

    public void RunFunc(Func<int, float> f, int v) => f(v);
}