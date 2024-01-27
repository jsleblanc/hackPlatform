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
}