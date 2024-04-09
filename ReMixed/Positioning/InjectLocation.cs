using System;

namespace ReMixed.Positioning;

public record InjectLocation(
    InjectTarget Target,
    bool Cancellable = false,
    InjectLocation.Shift ShiftBy = InjectLocation.Shift.BeforeArguments,
    int Idx = 0,
    bool CaptureInstance = true,
    bool CaptureArgs = true) {
    
    public enum Shift {
        BeforeArguments,
        Before,
        After,
    }

    // Ugly, but theres no reason to make `Shift` a class
    public static bool ShiftReplacesInstr(Shift shift) {
        return shift switch {
            Shift.BeforeArguments => true,
            Shift.Before => true,
            Shift.After => false,
            _ => throw new ArgumentOutOfRangeException(nameof(shift), shift, null)
        };
    }
}