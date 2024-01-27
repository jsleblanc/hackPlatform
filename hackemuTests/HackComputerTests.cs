using hackemu;

namespace hackemuTests;

public class HackComputerTests
{
    [Fact]
    public void ShouldSetARegisterTo1()
    {
        var hc = new HackComputer(new[] { "1110111111100000" });
        hc.ComputeNext();
        Assert.Equal(1, hc.A);
    }
}