using System;

namespace ReMixed;

public class MergedAttribute(string origName) : Attribute {
    public string? OrigName { get; } = origName;
}