using System;
using Mono.Cecil;
using Mono.Cecil.Cil;
using Mono.Collections.Generic;

namespace ReMixed;

// Cecil's ILProcessor is tied to a method body, sometimes we require working on sub-collections of instructions
public sealed class CollectionILProcessor {
    private readonly Collection<Instruction> instructions;
    private readonly IBodyDataProvider body;
    public int Count => instructions.Count;

    public CollectionILProcessor(Collection<Instruction> instructions, IBodyDataProvider body) {
        this.instructions = instructions;
        this.body = body;
    }

    public Instruction Create(OpCode opcode) {
        return Instruction.Create(opcode);
    }

    public Instruction Create(OpCode opcode, TypeReference type) {
        return Instruction.Create(opcode, type);
    }

    public Instruction Create(OpCode opcode, CallSite site) {
        return Instruction.Create(opcode, site);
    }

    public Instruction Create(OpCode opcode, MethodReference method) {
        return Instruction.Create(opcode, method);
    }

    public Instruction Create(OpCode opcode, FieldReference field) {
        return Instruction.Create(opcode, field);
    }

    public Instruction Create(OpCode opcode, string value) {
        return Instruction.Create(opcode, value);
    }

    public Instruction Create(OpCode opcode, sbyte value) {
        return Instruction.Create(opcode, value);
    }

    public Instruction Create(OpCode opcode, byte value) {
        if (opcode.OperandType == OperandType.ShortInlineVar)
            return Instruction.Create(opcode, body.Variables[value]);

        if (opcode.OperandType == OperandType.ShortInlineArg)
            return Instruction.Create(opcode, body.GetParameter(value));

        return Instruction.Create(opcode, value);
    }

    public Instruction Create(OpCode opcode, int value) {
        if (opcode.OperandType == OperandType.InlineVar)
            return Instruction.Create(opcode, body.Variables[value]);

        if (opcode.OperandType == OperandType.InlineArg)
            return Instruction.Create(opcode, body.GetParameter(value));

        return Instruction.Create(opcode, value);
    }

    public Instruction Create(OpCode opcode, long value) {
        return Instruction.Create(opcode, value);
    }

    public Instruction Create(OpCode opcode, float value) {
        return Instruction.Create(opcode, value);
    }

    public Instruction Create(OpCode opcode, double value) {
        return Instruction.Create(opcode, value);
    }

    public Instruction Create(OpCode opcode, Instruction target) {
        return Instruction.Create(opcode, target);
    }

    public Instruction Create(OpCode opcode, Instruction[] targets) {
        return Instruction.Create(opcode, targets);
    }

    public Instruction Create(OpCode opcode, VariableDefinition variable) {
        return Instruction.Create(opcode, variable);
    }

    public Instruction Create(OpCode opcode, ParameterDefinition parameter) {
        return Instruction.Create(opcode, parameter);
    }

    public void Emit(OpCode opcode) {
        Append(Create(opcode));
    }

    public void Emit(OpCode opcode, TypeReference type) {
        Append(Create(opcode, type));
    }

    public void Emit(OpCode opcode, MethodReference method) {
        Append(Create(opcode, method));
    }

    public void Emit(OpCode opcode, CallSite site) {
        Append(Create(opcode, site));
    }

    public void Emit(OpCode opcode, FieldReference field) {
        Append(Create(opcode, field));
    }

    public void Emit(OpCode opcode, string value) {
        Append(Create(opcode, value));
    }

    public void Emit(OpCode opcode, byte value) {
        Append(Create(opcode, value));
    }

    public void Emit(OpCode opcode, sbyte value) {
        Append(Create(opcode, value));
    }

    public void Emit(OpCode opcode, int value) {
        Append(Create(opcode, value));
    }

    public void Emit(OpCode opcode, long value) {
        Append(Create(opcode, value));
    }

    public void Emit(OpCode opcode, float value) {
        Append(Create(opcode, value));
    }

    public void Emit(OpCode opcode, double value) {
        Append(Create(opcode, value));
    }

    public void Emit(OpCode opcode, Instruction target) {
        Append(Create(opcode, target));
    }

    public void Emit(OpCode opcode, Instruction[] targets) {
        Append(Create(opcode, targets));
    }

    public void Emit(OpCode opcode, VariableDefinition variable) {
        Append(Create(opcode, variable));
    }

    public void Emit(OpCode opcode, ParameterDefinition parameter) {
        Append(Create(opcode, parameter));
    }

    public void InsertBefore(Instruction target, Instruction instruction) {
        if (target == null)
            throw new ArgumentNullException(nameof(target));
        if (instruction == null)
            throw new ArgumentNullException(nameof(instruction));

        var index = instructions.IndexOf(target);
        if (index == -1)
            throw new ArgumentOutOfRangeException(nameof(target));

        instructions.Insert(index, instruction);
    }

    public void InsertAfter(Instruction target, Instruction instruction) {
        if (target == null)
            throw new ArgumentNullException(nameof(target));
        if (instruction == null)
            throw new ArgumentNullException(nameof(instruction));

        var index = instructions.IndexOf(target);
        if (index == -1)
            throw new ArgumentOutOfRangeException(nameof(target));

        instructions.Insert(index + 1, instruction);
    }

    public void InsertAfter(int index, Instruction instruction) {
        if (index < 0 || index >= instructions.Count)
            throw new ArgumentOutOfRangeException(nameof(index));
        if (instruction == null)
            throw new ArgumentNullException(nameof(instruction));

        instructions.Insert(index + 1, instruction);
    }

    public void Append(Instruction instruction) {
        if (instruction == null)
            throw new ArgumentNullException(nameof(instruction));

        instructions.Add(instruction);
    }

    public void Replace(Instruction target, Instruction instruction) {
        if (target == null)
            throw new ArgumentNullException(nameof(target));
        if (instruction == null)
            throw new ArgumentNullException(nameof(instruction));

        InsertAfter(target, instruction);
        Remove(target);
    }

    public void Replace(int index, Instruction instruction) {
        if (instruction == null)
            throw new ArgumentNullException(nameof(instruction));

        InsertAfter(index, instruction);
        RemoveAt(index);
    }

    public void Remove(Instruction instruction) {
        if (instruction == null)
            throw new ArgumentNullException(nameof(instruction));

        if (!instructions.Remove(instruction))
            throw new ArgumentOutOfRangeException(nameof(instruction));
    }

    public void RemoveAt(int index) {
        if (index < 0 || index >= instructions.Count)
            throw new ArgumentOutOfRangeException(nameof(index));

        instructions.RemoveAt(index);
    }

    public void Clear() {
        instructions.Clear();
    }

    public interface IBodyDataProvider {
        public Collection<VariableDefinition> Variables { get; }
        public ParameterDefinition? GetParameter(int index);
        public TypeReference GetReturnType();

        public void AddVariable(VariableDefinition variableDefinition);
        
        public bool HasThis { get; }
    }
}