using System;
using System.Collections.Generic;
using System.ComponentModel.Design;
using System.Linq;
using ReMixed.Registry;

namespace ReMixed.Positioning;

public abstract class Positioner {
    private static Dictionary<string, Type>? ids;

    private static Dictionary<string, Type> Ids {
        get {
            if (ids != null) return ids;
            ids = new Dictionary<string, Type>();

            foreach (Type[] types in AppDomain.CurrentDomain.GetAssemblies()
                         .Select(a => a.GetTypes())) {
                foreach (Type type in types) {
                    object[] attrs = type.GetCustomAttributes(typeof(AtIdAttribute), true);
                    if (attrs.Length == 0) continue;
                    foreach (AtIdAttribute atIdAttribute in attrs) {
                        if (ids.TryGetValue(atIdAttribute.Id, out Type? conflictType)) {
                            throw new Exception($"Id conflict with type {type.FullName} and {conflictType.FullName} for Id: {atIdAttribute.Id}");
                        }
                        ids[atIdAttribute.Id] = type;
                    }
                }
                
            }
            
            return ids;
        }
    }

    public static Positioner FromAttribute(AtAttribute atAttribute) {
        if (!Ids.TryGetValue(atAttribute.Value, out Type? positionerType)) {
            throw new Exception($"Positioner for id {atAttribute.Value} not found!");
        }

        object? instance = Activator.CreateInstance(positionerType);
        if (instance == null) throw new Exception($"Could not create instance of positioner: {positionerType.FullName}");

        return (Positioner) instance;
    }

    public abstract void Do(MethodPatchContext.Positioner positioner);

}