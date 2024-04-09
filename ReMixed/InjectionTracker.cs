using System;
using System.Collections.Generic;
using Mono.Cecil.Cil;

namespace ReMixed;

public class InjectionTracker {
    private readonly MethodBody origBody;
    private readonly StackAnalysis stAnalysis;
    private readonly List<InjectionData> injections = [];
    public Dictionary<int, bool> Retargeted { get; } = new();
    
    public InjectionTracker(MethodBody body, StackAnalysis stackAnalysis) {
        origBody = body;
        stAnalysis = stackAnalysis;
    }

    /// <summary>
    /// Registers an injection.
    /// </summary>
    /// <param name="modified">The modified MethodBody where the injection is present.</param>
    /// <param name="startIdx">The start index of the injection in the modified MethodBody.</param>
    /// <param name="size">The size of the injection.</param>
    /// <param name="afterInstr">Whether the injection is attached to the previous instruction (true) or the next one (false).</param>
    public InjectionData RegisterInjection(MethodBody modified, int startIdx, int size, bool afterInstr) {
        // Try merging first
        for (int i = 0; i < injections.Count; i++) {
            if (injections[i].AfterInstr == afterInstr && injections[i].StartIdx + injections[i].Size == startIdx) {
                injections[i] = new InjectionData(injections[i].StartIdx, injections[i].Size + size, afterInstr);
                // if (!VerifyInjection(modified, injections[i]))
                //     throw new NotSupportedException("Invalid injection data!");
                return injections[i];
            }
        }
        // Otherwise add a new entry
        injections.Add(new InjectionData(startIdx, size, afterInstr));
        if (!VerifyInjection(modified, injections[^1]))
            throw new NotSupportedException("Invalid injection data.");
        return injections[^1];
    }

    // Check if the prev injection instr and post injection instr match in orig and in modified
    private bool VerifyInjection(MethodBody modified, InjectionData data) {

        int origIndex = CalculateOrigIndex(data.StartIdx);
        int shift = data.AfterInstr ? 0 : 1; // CalculateOrigIndex may return a different index depending on the inj type
        if (data.StartIdx != 0) { // Cant check if injection is at the start
            if (!modified.Instructions[data.StartIdx - 1]
                    .InstrEquals(
                        origBody.Instructions[origIndex - shift], stAnalysis, modified, origBody))
                return false;
        }

        if (origIndex + 1 < origBody.Instructions.Count) { // Injects until last instr, cant check after
            if (!modified.Instructions[data.StartIdx + data.Size]
                    .InstrEquals(
                        origBody.Instructions[origIndex - shift + 1], stAnalysis, modified, origBody))
                return false;
        }

        return true;
    }

    /// <summary>
    /// Obtains where would the current index in the modified body fall in the orig body.
    /// Inverse of `CalculateModifiedIndex`.
    /// </summary>
    /// <param name="index">The index in modified method.</param>
    /// <returns>An index in the orig body.</returns>
    public int CalculateOrigIndex(int index) {
        int shifts = 0;
        for (int i = 0; i < injections.Count; i++) {
            if (injections[i].StartIdx + injections[i].Size <= index) {
                shifts += injections[i].Size;
            } else if (injections[i].StartIdx <= index) {
                shifts += index - injections[i].StartIdx - (injections[i].AfterInstr ? 1 : 0);
            }
        }

        return index - shifts;
    }

    /// <summary>
    /// Obtains where would the current index in the orig body fall in the modified body.
    /// Inverse of `CalculateOrigIndex`.
    /// </summary>
    /// <param name="index">The index in the orig method.</param>
    /// <returns>An index in the modified method.</returns>
    public int CalculateModifiedIndex(int index) {
        int newIndex = index;
        for (int i = 0; i < injections.Count; i++) {
            if (injections[i].StartIdx <= index) {
                newIndex += injections[i].Size;
            }
        }

        return newIndex;
    }

    public bool IsInInjection(int index) {
        foreach (InjectionData injection in injections) {
            if (injection.StartIdx <= index && injection.StartIdx + injection.Size > index)
                return true;
        }

        return false;
    }

    public int NextFreeIndex(int index) {
        foreach (InjectionData injection in injections) {
            if (injection.StartIdx <= index && injection.StartIdx + injection.Size > index)
                return injection.StartIdx + injection.Size;
        }

        return index;
    }

    public struct InjectionData(int startIdx, int size, bool afterInstr) {
        public readonly int StartIdx = startIdx;
        public readonly int Size = size;
        public readonly bool AfterInstr = afterInstr;
    }
}