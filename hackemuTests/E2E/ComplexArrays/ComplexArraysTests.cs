using FluentAssertions;
using hackemu;

namespace hackemuTests.E2E.ComplexArrays;

public class ComplexArraysTests
{
    [Fact]
    public void ComplexArraysTest()
    {
        var binaryCode =
            CompilerWorkflow.CompileFiles(new[] { new FileInfo("E2E/ComplexArrays/Main.jack") });
        
        var hackComputer = new HackComputer(binaryCode);
        hackComputer.ComputeCycles(1_000_000);

        hackComputer.Memory(8001).Should().Be(5);
        hackComputer.Memory(8002).Should().Be(40);
        hackComputer.Memory(8003).Should().Be(0);
        hackComputer.Memory(8004).Should().Be(77);
        hackComputer.Memory(8005).Should().Be(110);
    } 
}