using System;

namespace ReMixed.Registry;

public class AtIdAttribute(string id) : Attribute {
    public string Id { get; } = id;
}