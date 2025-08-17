using System.Reflection;

namespace ReMixed.Injection;

public record struct InjectorID(string Id);

public static class InjectorIds {
    // Everything in here will be initialized through reflection, just make the ide shut up
#pragma warning disable CS8618 // Non-nullable field must contain a non-null value when exiting constructor. Consider declaring as nullable.
    // ReSharper disable UnassignedField.Global
    // ReSharper disable UnusedAutoPropertyAccessor.Local
    public static InjectorID CIInjectorNonCancellable { get; private set; }
    public static InjectorID CIInjectorCancellable { get; private set; }
    public static InjectorID InstanceInjector { get; private set; }
    // ReSharper enable UnassignedField.Global
    // ReSharper enable UnusedAutoPropertyAccessor.Local
    
    static InjectorIds() {
        // Initialize everything with its own id through reflection
        PropertyInfo[] properties = typeof(InjectorIds).GetProperties(BindingFlags.Static | BindingFlags.Public);
        foreach (PropertyInfo property in properties) {
            if (property.PropertyType == typeof(InjectorID)) {
                property.SetValue(null, new InjectorID(property.Name));
            }
        }
    }
#pragma warning restore CS8618 // Non-nullable field must contain a non-null value when exiting constructor. Consider declaring as nullable.
}