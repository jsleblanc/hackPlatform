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
    }
    
    [Fact]
    public void ShouldSetDRegisterTo1()
    {
        var hc = new HackComputer(new[] { "1110111111010000" });
        hc.ComputeNext();
        Assert.Equal(0, hc.A);
        Assert.Equal(1, hc.D);
    }    
}