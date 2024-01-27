using hackemu;

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
    /// Uses the Mult.asm from Chapter 4
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
        hc.ComputeCycles(250);
        
        Assert.Equal(expected, hc.Memory(2));
    }
}