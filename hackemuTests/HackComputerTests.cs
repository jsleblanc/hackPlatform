using FluentAssertions;
using hackemu;
using static hackemu.HackComputer;

namespace hackemuTests;

public class HackComputerTests
{
    [Fact]
    public void ShouldSetARegisterTo1()
    {
        var hc = new HackComputer(new[] { "1110111111100000" });
        hc.ComputeNext();
        Assert.Equal(0, hc.D);
        Assert.Equal(1, hc.A);
        Assert.NotEqual(0, hc.PC);
    }
    
    [Fact]
    public void ShouldSetDRegisterTo1()
    {
        var hc = new HackComputer(new[] { "1110111111010000" });
        hc.ComputeNext();
        Assert.Equal(0, hc.A);
        Assert.Equal(1, hc.D);
        Assert.NotEqual(0, hc.PC);
    }

    [Fact]
    public void ShouldSetAAndDRegistersToNeg1()
    {
        var hc = new HackComputer(new[] { "1110111010110000" });
        hc.ComputeNext();
        Assert.Equal(-1, hc.D);
        Assert.Equal(-1, hc.A);
        Assert.NotEqual(0, hc.PC);
    }
    
    /// <summary>
    /// @5
    /// D=A
    /// @10
    /// M=D
    /// </summary>
    [Fact]
    public void ShouldWriteValueToMemoryAtSpecifiedAddress()
    {
        var hc = new HackComputer(new[]
        {
            "0000000000000101",
            "1110110000010000",
            "0000000000001010",
            "1110001100001000"
        });
        hc.ComputeNext();
        hc.ComputeNext();
        hc.ComputeNext();
        hc.ComputeNext();

        Assert.Equal(5, hc.M);
        Assert.Equal(5, hc.Memory(10));
        Assert.NotEqual(0, hc.PC);
    }

    /// <summary>
    /// Runs a small assembly program that multiplies two numbers and writes the result to RAM
    /// Uses the Mult.asm program from chapter 4
    /// </summary>
    /// <param name="x"></param>
    /// <param name="y"></param>
    /// <param name="expected"></param>
    [Theory]
    [InlineData(0, 0, 0)]
    [InlineData(1, 0, 0)]
    [InlineData(0, 2, 0)]
    [InlineData(3, 1, 3)]
    [InlineData(2, 4, 8)]
    [InlineData(6, 7, 42)]
    public void ShouldMultiplyTwoNumbers(short x, short y, short expected)
    {
        var hc = new HackComputer(new[]
        {
            "0000000000000010",
            "1110101010001000",
            "0000000000010000",
            "1110101010001000",
            "0000000000010001",
            "1110101010001000",
            "0000000000000000",
            "1111110000010000",
            "0000000000010000",
            "1110001100001000",
            "0000000000000000",
            "1111110000010000",
            "0000000000100000",
            "1110001100000010",
            "0000000000000001",
            "1111110000010000",
            "0000000000100000",
            "1110001100000010",
            "0000000000000001",
            "1111110000010000",
            "0000000000010001",
            "1111000010001000",
            "0000000000010000",
            "1111110010001000",
            "0000000000010000",
            "1111110000010000",
            "0000000000010010",
            "1110001100000001",
            "0000000000010001",
            "1111110000010000",
            "0000000000000010",
            "1110001100001000",
            "0000000000100000",
            "1110101010000111"
        });
        hc.SetMemory(0, x);
        hc.SetMemory(1, y);
        hc.ComputeCycles(100);
        
        Assert.Equal(expected, hc.Memory(2));
    }

    /// <summary>
    /// Runs a small assembly program that computes the max of two numbers and writes the result to RAM
    /// Uses the Max.asm program from chapter 6
    /// </summary>
    /// <param name="x"></param>
    /// <param name="y"></param>
    /// <param name="expected"></param>
    [Theory]
    [InlineData(0, 0, 0)]
    [InlineData(0, 1, 1)]
    [InlineData(1, 0, 1)]
    [InlineData(1, 2, 2)]
    [InlineData(2, 1, 2)]
    [InlineData(6, 7, 7)]
    public void ShouldFindMaxOfTwoNumbers(short x, short y, short expected)
    {
        var hc = new HackComputer(new[]
        {
            "0000000000000000",
            "1111110000010000",
            "0000000000000001",
            "1111010011010000",
            "0000000000001100",
            "1110001100000001",
            "0000000000000001",
            "1111110000010000",
            "0000000000000010",
            "1110001100001000",
            "0000000000010000",
            "1110101010000111",
            "0000000000000000",
            "1111110000010000",
            "0000000000000010",
            "1110001100001000",
            "0000000000010000",
            "1110101010000111"
        });
        hc.SetMemory(0, x);
        hc.SetMemory(1, y);
        hc.ComputeCycles(100);

        Assert.Equal(expected, hc.Memory(2));
    }

    [Theory]
    [InlineData(0b100_0_000000_000_000, OpCode.OP_D_AND_A, Destination.None, Jump.None)]
    [InlineData(0b100_0_000010_000_000, OpCode.OP_D_PLUS_A, Destination.None, Jump.None)]
    [InlineData(0b100_0_000111_000_000, OpCode.OP_A_MINUS_D, Destination.None, Jump.None)]
    [InlineData(0b100_0_001100_000_000, OpCode.OP_D, Destination.None, Jump.None)]
    [InlineData(0b100_0_001101_000_000, OpCode.OP_NOT_D, Destination.None, Jump.None)]
    [InlineData(0b100_0_001110_000_000, OpCode.OP_D_MINUS_ONE, Destination.None, Jump.None)]
    [InlineData(0b100_0_001111_000_000, OpCode.OP_NEG_D, Destination.None, Jump.None)]
    [InlineData(0b100_0_010011_000_000, OpCode.OP_D_MINUS_A, Destination.None, Jump.None)]
    [InlineData(0b100_0_010101_000_000, OpCode.OP_D_OR_A, Destination.None, Jump.None)]
    [InlineData(0b100_0_011111_000_000, OpCode.OP_D_PLUS_ONE, Destination.None, Jump.None)]
    [InlineData(0b100_0_101010_000_000, OpCode.OP_ZERO, Destination.None, Jump.None)]
    [InlineData(0b100_0_110000_000_000, OpCode.OP_A, Destination.None, Jump.None)]
    [InlineData(0b100_0_110001_000_000, OpCode.OP_NOT_A, Destination.None, Jump.None)]
    [InlineData(0b100_0_110010_000_000, OpCode.OP_A_MINUS_ONE, Destination.None, Jump.None)]
    [InlineData(0b100_0_110011_000_000, OpCode.OP_NEG_A, Destination.None, Jump.None)]
    [InlineData(0b100_0_110111_000_000, OpCode.OP_A_PLUS_ONE, Destination.None, Jump.None)]
    [InlineData(0b100_0_111010_000_000, OpCode.OP_NEG_ONE, Destination.None, Jump.None)]
    [InlineData(0b100_0_111111_000_000, OpCode.OP_ONE, Destination.None, Jump.None)]
    [InlineData(0b100_1_000000_000_000, OpCode.OP_D_AND_M, Destination.None, Jump.None)]
    [InlineData(0b100_1_000010_000_000, OpCode.OP_D_PLUS_M, Destination.None, Jump.None)]
    [InlineData(0b100_1_000111_000_000, OpCode.OP_M_MINUS_D, Destination.None, Jump.None)]
    [InlineData(0b100_1_010011_000_000, OpCode.OP_D_MINUS_M, Destination.None, Jump.None)]
    [InlineData(0b100_1_010101_000_000, OpCode.OP_D_OR_M, Destination.None, Jump.None)]
    [InlineData(0b100_1_110000_000_000, OpCode.OP_M, Destination.None, Jump.None)]
    [InlineData(0b100_1_110001_000_000, OpCode.OP_NOT_M, Destination.None, Jump.None)]
    [InlineData(0b100_1_110010_000_000, OpCode.OP_M_MINUS_ONE, Destination.None, Jump.None)]
    [InlineData(0b100_1_110111_000_000, OpCode.OP_M_PLUS_ONE, Destination.None, Jump.None)]
    public void ShouldDecodeInstruction(ushort instruction, OpCode code, Destination dest, Jump jmp)
    {
        var (destination, opCode, jump) = Decode_C_Instruction((short)instruction);
        destination.Should().Be(dest);
        opCode.Should().Be(code);
        jump.Should().Be(jmp);
    }

    [Theory]
    [InlineData(0b100_0_000000_000_000)]
    [InlineData(0b100_0_000010_000_000)]
    [InlineData(0b100_0_000111_000_000)]
    [InlineData(0b100_0_001100_000_000)]
    [InlineData(0b100_0_001101_000_000)]
    [InlineData(0b100_0_001110_000_000)]
    [InlineData(0b100_0_001111_000_000)]
    [InlineData(0b100_0_010011_000_000)]
    [InlineData(0b100_0_010101_000_000)]
    [InlineData(0b100_0_011111_000_000)]
    [InlineData(0b100_0_101010_000_000)]
    [InlineData(0b100_0_110000_000_000)]
    [InlineData(0b100_0_110001_000_000)]
    [InlineData(0b100_0_110010_000_000)]
    [InlineData(0b100_0_110011_000_000)]
    [InlineData(0b100_0_110111_000_000)]
    [InlineData(0b100_0_111010_000_000)]
    [InlineData(0b100_0_111111_000_000)]
    [InlineData(0b100_1_000000_000_000)]
    [InlineData(0b100_1_000010_000_000)]
    [InlineData(0b100_1_000111_000_000)]
    [InlineData(0b100_1_010011_000_000)]
    [InlineData(0b100_1_010101_000_000)]
    [InlineData(0b100_1_110000_000_000)]
    [InlineData(0b100_1_110001_000_000)]
    [InlineData(0b100_1_110010_000_000)]
    [InlineData(0b100_1_110111_000_000)]
    public void ShouldBeComputationInstructions(ushort instruction)
    {
        Is_C_Instruction((short)instruction).Should().BeTrue();
    }

    [Fact]
    public void ShouldWriteToMemoryDestinationBeforeRegisters()
    {
        var hc = new HackVirtualMachine(new[]
        {
            "0000000011111111",
            "1110111111001000",
            "0000000100000000",
            "1110110000010000",
            "0000000000000000",
            "1110001100001000",
            "0000000000000000",
            "1111110010101000",
            "1111110000010000"
        });
        hc.ComputeCycles(9);
        hc.D.Should().Be(1);
        hc.A.Should().Be(255);
        hc.SP.Should().Be(255);
    }
}