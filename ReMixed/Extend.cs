using System;
using System.Runtime.CompilerServices;

namespace ReMixed;

public class ExtendManager {
    private static readonly ConditionalWeakTable<object, Extends> Table = new();

    public static T GetEntry<T, V>(V target, Func<V, T> create) where T : Extends where V : class {
        if (Table.TryGetValue(target, out Extends? value)) {
            return Unsafe.As<T>(value);
        }
        Console.WriteLine($"WARNING: Object of type {typeof(V)} was not properly registered, this may lead to undefined behavior!");

        return (T)AddEntry(target, create(target));
    }

    public static Extends AddEntry(object target, Extends value) {
        Table.Add(target, value);
        return value;
    }
}

public abstract class Extends {
}

public abstract class Extends<T> : Extends {

    protected T _this { get; private set; }

    public Extends(T obj) {
        SetRef(obj);
    }

    public void SetRef(T obj) => _this = obj;

}