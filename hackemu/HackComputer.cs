namespace hackemu;

/// <summary>
/// Emulates the Hack Platform CPU
/// </summary>
public class HackComputer
{
    private readonly short[] _rom = new short[0xFFFF];
    private readonly short[] _ram = new short[0xFFFF];

    private short _PC;
    private short _A;
    private short _D;

    [Flags]
    private enum Destination
    {
        None = 0,
        M = 1,
        D = 2,
        A = 4
    }

    private enum Jump
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

    private enum OpCode
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
        programCode.CopyTo(_rom, 0);
        _ram.Initialize();
        _PC = 0;
        _A = 0;
        _D = 0;
    }

    public short A => _A;
    public short D => _D;
    public short M => _ram[_A];
    public short PC => _PC;
    public short Memory(short index) => _ram[index];
    
    public bool ComputeNext()
    {
        var instruction = _rom[_PC];
        if (Is_C_Instruction(instruction))
        {
            var (destination, opCode, jump) = Decode_C_Instruction(instruction);

            short compResult = opCode switch
            {
                OpCode.OP_ZERO => 0,
                OpCode.OP_ONE => 1,
                OpCode.OP_NEG_ONE => -1,
                OpCode.OP_D => _D,
                OpCode.OP_A => _A,
                OpCode.OP_M => _ram[_A],
                OpCode.OP_NOT_D => (short)~_D,
                OpCode.OP_NOT_A => (short)~_A,
                OpCode.OP_NOT_M => (short)~_ram[_A],
                OpCode.OP_NEG_D => (short)-_D,
                OpCode.OP_NEG_A => (short)-_A,
                OpCode.OP_NEG_M => (short)-_ram[_A],
                OpCode.OP_D_PLUS_ONE => (short)(_D + 1),
                OpCode.OP_A_PLUS_ONE => (short)(_A + 1),
                OpCode.OP_M_PLUS_ONE => (short)(_ram[_A] + 1),
                OpCode.OP_D_MINUS_ONE => (short)(_D - 1),
                OpCode.OP_A_MINUS_ONE => (short)(_A - 1),
                OpCode.OP_M_MINUS_ONE => (short)(_ram[_A] - 1),
                OpCode.OP_D_PLUS_A => (short)(_D + _A),
                OpCode.OP_D_PLUS_M => (short)(_D + _ram[_A]),
                OpCode.OP_D_MINUS_A => (short)(_D - _A),
                OpCode.OP_D_MINUS_M => (short)(_D - _ram[_A]),
                OpCode.OP_A_MINUS_D => (short)(_A - _D),
                OpCode.OP_M_MINUS_D => (short)(_ram[_A] - _D),
                OpCode.OP_D_AND_A => (short)(_D & _A),
                OpCode.OP_D_AND_M => (short)(_D & _ram[_A]),
                OpCode.OP_D_OR_A => (short)(_D | _A),
                OpCode.OP_D_OR_M => (short)(_D | _ram[_A]),
                _ => throw new ArgumentOutOfRangeException(nameof(opCode))
            };

            if (destination.HasFlag(Destination.A))
            {
                _A = compResult;
            }

            if (destination.HasFlag(Destination.M))
            {
                _ram[_A] = compResult;
            }

            if (destination.HasFlag(Destination.D))
            {
                _D = compResult;
            }

            switch (jump)
            {
                case Jump.None:
                    _PC++;
                    break;
                case Jump.JGT:
                    if (compResult > 0)
                    {
                        _PC = _A;
                    }
                    break;
                case Jump.JEQ:
                    if (compResult == 0)
                    {
                        _PC = _A;
                    }
                    break;
                case Jump.JGE:
                    if (compResult >= 0)
                    {
                        _PC = _A;
                    }
                    break;
                case Jump.JLT:
                    if (compResult < 0)
                    {
                        _PC = _A;
                    }
                    break;
                case Jump.JNE:
                    if (compResult != 0)
                    {
                        _PC = _A;
                    }
                    break;
                case Jump.JLE:
                    if (compResult <= 0)
                    {
                        _PC = _A;
                    }
                    break;
                case Jump.JMP:
                    _PC = _A;
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(jump));
            }
        }
        else
        {
            _A = instruction;
        }
        
        return true;
    }

    private static bool Is_C_Instruction(short instruction)
    {
        const ushort cInstructionMask = 0b111_0000000000000;
        var x = instruction & cInstructionMask;
        return x == cInstructionMask;
    }

    private static (Destination, OpCode, Jump) Decode_C_Instruction(short instruction)
    {
        const ushort opCodeMask = 0b111_0000000111111;
        const ushort destinationMask = 0b111_1111111000111;
        const ushort jumpMask = 0b111_1111111111000;

        var dest = instruction ^ destinationMask;
        var jump = instruction ^ jumpMask;
        var opCode = instruction ^ opCodeMask;

        return ((Destination)dest, (OpCode)opCode, (Jump)jump);
    }
}