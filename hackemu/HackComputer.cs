﻿using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;

namespace hackemu;

/// <summary>
/// Emulates the Hack Platform CPU
/// </summary>
public class HackComputer
{
    private readonly short[] _rom = new short[0xFFFF];
    private readonly short[] _ram = new short[0xFFFF];

    private ushort _pc;
    private short _aReg;
    private short _dReg;

    [Flags]
    public enum Destination
    {
        None = 0,
        M = 1,
        D = 2,
        A = 4
    }

    [SuppressMessage("ReSharper", "InconsistentNaming")]
    public enum Jump
    {
        None = 0,
        JGT = 1,
        JEQ = 2,
        JGE = 3,
        JLT = 4,
        JNE = 5,
        JLE = 6,
        JMP = 7
    }

    [SuppressMessage("ReSharper", "InconsistentNaming")]
    public enum OpCode
    {
        OP_ZERO = 0b0_101010,
        OP_ONE = 0b0_111111,
        OP_NEG_ONE = 0b0_111010,
        OP_D = 0b0_001100,
        OP_A = 0b0_110000,
        OP_M = 0b1_110000,
        OP_NOT_D = 0b0_001101,
        OP_NOT_A = 0b0_110001,
        OP_NOT_M = 0b1_110001,
        OP_NEG_D = 0b0_001111,
        OP_NEG_A = 0b0_110011,
        OP_NEG_M = 0b1_110011,
        OP_D_PLUS_ONE = 0b0_011111,
        OP_A_PLUS_ONE = 0b0_110111,
        OP_M_PLUS_ONE = 0b1_110111,
        OP_D_MINUS_ONE = 0b0_001110,
        OP_A_MINUS_ONE = 0b0_110010,
        OP_M_MINUS_ONE = 0b1_110010,
        OP_D_PLUS_A = 0b0_000010,
        OP_D_PLUS_M = 0b1_000010,
        OP_D_MINUS_A = 0b0_010011,
        OP_D_MINUS_M = 0b1_010011,
        OP_A_MINUS_D = 0b0_000111,
        OP_M_MINUS_D = 0b1_000111,
        OP_D_AND_A = 0b0_000000,
        OP_D_AND_M = 0b1_000000,
        OP_D_OR_A = 0b0_010101,
        OP_D_OR_M = 0b1_010101
    }

    public HackComputer(IEnumerable<string> programCode)
        : this(programCode.Select(s => Convert.ToInt16(s, 2)).ToArray())
    { }

    public HackComputer(short[] programCode)
    {
        for (var index = 0; index < programCode.Length; index++)
            ValidateInstruction(index, programCode[index]);
        
        programCode.CopyTo(_rom, 0);
        _ram.Initialize();
        _pc = 0;
        _aReg = 0;
        _dReg = 0;
    }

    public short A => _aReg;
    public short D => _dReg;
    public short M => _ram[_aReg];
    public ushort PC => _pc;
    public short Memory(ushort index) => _ram[index];

    public void SetMemory(ushort index, short value) => _ram[index] = value;
    
    public void ComputeCycles(int count)
    {
        for (var x = 0; x < count; x++)
        {
            ComputeNext();
            if (CheckHalted()) break;
        }
    }

    /// <summary>
    /// Computes current instruction
    /// </summary>
    /// <exception cref="ArgumentOutOfRangeException"></exception>
    public void ComputeNext()
    {
        var instruction = _rom[_pc];
        if (Is_C_Instruction(instruction))
        {
            var (destination, opCode, jump) = Decode_C_Instruction(instruction);

            short compResult = opCode switch
            {
                OpCode.OP_ZERO => 0,
                OpCode.OP_ONE => 1,
                OpCode.OP_NEG_ONE => -1,
                OpCode.OP_D => _dReg,
                OpCode.OP_A => _aReg,
                OpCode.OP_M => _ram[_aReg],
                OpCode.OP_NOT_D => (short)~_dReg,
                OpCode.OP_NOT_A => (short)~_aReg,
                OpCode.OP_NOT_M => (short)~_ram[_aReg],
                OpCode.OP_NEG_D => (short)-_dReg,
                OpCode.OP_NEG_A => (short)-_aReg,
                OpCode.OP_NEG_M => (short)-_ram[_aReg],
                OpCode.OP_D_PLUS_ONE => (short)(_dReg + 1),
                OpCode.OP_A_PLUS_ONE => (short)(_aReg + 1),
                OpCode.OP_M_PLUS_ONE => (short)(_ram[_aReg] + 1),
                OpCode.OP_D_MINUS_ONE => (short)(_dReg - 1),
                OpCode.OP_A_MINUS_ONE => (short)(_aReg - 1),
                OpCode.OP_M_MINUS_ONE => (short)(_ram[_aReg] - 1),
                OpCode.OP_D_PLUS_A => (short)(_dReg + _aReg),
                OpCode.OP_D_PLUS_M => (short)(_dReg + _ram[_aReg]),
                OpCode.OP_D_MINUS_A => (short)(_dReg - _aReg),
                OpCode.OP_D_MINUS_M => (short)(_dReg - _ram[_aReg]),
                OpCode.OP_A_MINUS_D => (short)(_aReg - _dReg),
                OpCode.OP_M_MINUS_D => (short)(_ram[_aReg] - _dReg),
                OpCode.OP_D_AND_A => (short)(_dReg & _aReg),
                OpCode.OP_D_AND_M => (short)(_dReg & _ram[_aReg]),
                OpCode.OP_D_OR_A => (short)(_dReg | _aReg),
                OpCode.OP_D_OR_M => (short)(_dReg | _ram[_aReg]),
                _ => throw new ArgumentOutOfRangeException(nameof(opCode))
            };
            
            if (destination.HasFlag(Destination.M))
            {
                Debug.Assert(_aReg >= 0, "A-Reg cannot be negative when used as a memory address");
                _ram[_aReg] = compResult;
            }
            
            if (destination.HasFlag(Destination.A))
            {
                _aReg = compResult;
            }
            
            if (destination.HasFlag(Destination.D))
            {
                _dReg = compResult;
            }

            var pcNext = (ushort)(_pc + 1);
            _pc = jump switch
            {
                Jump.None => pcNext,
                Jump.JGT => compResult > 0 ? (ushort)_aReg : pcNext,
                Jump.JEQ => compResult == 0 ? (ushort)_aReg : pcNext,
                Jump.JGE => compResult >= 0 ? (ushort)_aReg : pcNext,
                Jump.JLT => compResult < 0 ? (ushort)_aReg : pcNext,
                Jump.JNE => compResult != 0 ? (ushort)_aReg : pcNext,
                Jump.JLE => compResult <= 0 ? (ushort)_aReg : pcNext,
                Jump.JMP => (ushort)_aReg,
                _ => throw new ArgumentOutOfRangeException(nameof(jump))
            };
        }
        else
        {
            _aReg = instruction;
            _pc++;
        }
    }

    public static bool Is_C_Instruction(short instruction)
    {
        const ushort cInstructionMask = 0b1000000000000000;
        var x = instruction & cInstructionMask;
        return x == cInstructionMask;
    }

    public static (Destination, OpCode, Jump) Decode_C_Instruction(short instruction)
    {
        const ushort opCodeMask = 0b0001111111000000;
        const ushort destinationMask = 0b0000000000111000;
        const ushort jumpMask = 0b0000000000000111;

        var dest = (instruction & destinationMask) >> 3;
        var jump = instruction & jumpMask;
        var opCode = (instruction & opCodeMask) >>> 6;

        return ((Destination)dest, (OpCode)opCode, (Jump)jump);
    }

    private static void ValidateInstruction(int index, short instruction)
    {
        if (!Is_C_Instruction(instruction)) return;

        var (destination, opCode, jump) = Decode_C_Instruction(instruction);
        if (!IsValid(destination))
            throw new ArgumentOutOfRangeException(nameof(instruction), 
                $"Instruction is invalid: destination. Position {index}; Value {instruction:x4}");

        if (!IsValid(opCode))
            throw new ArgumentOutOfRangeException(nameof(instruction), 
                $"Instruction is invalid: opCode. Position {index}; Value {instruction:x4}");

        if (!IsValid(jump))
            throw new ArgumentOutOfRangeException(nameof(instruction), 
                $"Instruction is invalid: jump. Position {index}; Value {instruction:x4}");
    }

    //I hate enums in C# so much
    //https://stackoverflow.com/questions/2674730/is-there-a-way-to-check-if-int-is-legal-enum-in-c
    private static bool IsValid<TEnum>(TEnum enumValue)
        where TEnum : struct
    {
        var firstChar = enumValue.ToString()[0];
        return (firstChar < '0' || firstChar > '9') && firstChar != '-';
    }

    private bool CheckHalted() => _ram[0x7FF] == 0x7A17;
}