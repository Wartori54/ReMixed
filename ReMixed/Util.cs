using System.Collections.Generic;
using Mono.Cecil;
using System;
using System.Reflection;

namespace ReMixed;

public interface IValueExtra<out TMain, out TExtra> where TMain : struct {
    TMain Get();
    
    TExtra? GetExtra();
}

public interface IMap<in TKey, TValue> where TKey : notnull {
    TValue this[TKey v] { get; set; }

    bool TryGetValue(TKey key, out TValue? value);
}

public sealed class ValueExtra<TMain, TExtra>(TMain value, TExtra? extra) : IValueExtra<TMain, TExtra>
    where TMain : struct {

    public static ValueExtra<T1, T2> FromValues<T1, T2>(T1 v1, T2 v2) where T1 : struct => new(v1, v2);

    public TMain Get() {
        return value;
    }
    
    public TExtra? GetExtra() {
        return extra;
    }
}

public sealed class MapDict<TKey, TValue> : IMap<TKey, TValue> where TKey : notnull {
    private readonly IDictionary<TKey, TValue> dict;

    public MapDict() {
        dict = new Dictionary<TKey, TValue>();
    }

    public MapDict(IDictionary<TKey, TValue> source) {
        dict = source;
    }
    
    public TValue this[TKey v] {
        get => dict[v];
        set => dict[v] = value;
    }
    
    public bool TryGetValue(TKey key, out TValue? value) {
        return dict.TryGetValue(key, out value);
    }
}

public class ExpandableSlot<T1, T2>(T1 first, T2 second) where T1 : new() where T2 : new() {
    public T1 First { get; set; } = first;
    public T2 Second { get; set; } = second;
    public ExpandableSlot() : this(new T1(), new T2()) { }
    public static implicit operator T1(ExpandableSlot<T1, T2> v) {
        return v.First;
    }
}

public class Util {
    public static Attribute? GetAttributeFromCecil(CustomAttribute customAttribute) {
        Type? attrType = Type.GetType(customAttribute.AttributeType.GetReflectionFullName());
        if (attrType == null) return null;
        if (attrType.ContainsGenericParameters) throw new ArgumentException("Cannot have generic attributes!", nameof(customAttribute));
        object?[] args = new object?[customAttribute.ConstructorArguments.Count];
        for (int i = 0; i < args.Length; i++) {
            args[i] = customAttribute.ConstructorArguments[i].Value;
        }
        object? attrObj = Activator.CreateInstance(attrType, args);
        return attrObj as Attribute;
    }
}