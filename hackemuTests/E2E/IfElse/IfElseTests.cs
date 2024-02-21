using FluentAssertions;
using hackemu;

namespace hackemuTests.E2E.IfElse;

public class IfElseTests
{
    [Fact]
    public void IfElseTest()
    {
        var binaryCode =
            CompilerWorkflow.CompileFiles(new[] { new FileInfo("E2E/IfElse/Main.jack") });
        
        var hackComputer = new HackComputer(binaryCode);
        hackComputer.SetMemory(8001, -1);
        hackComputer.SetMemory(8002, -1);
        hackComputer.ComputeCycles(10_000);

        hackComputer.Memory(8001).Should().Be(1, "Memory address: 8001");
        hackComputer.Memory(8002).Should().Be(2, "Memory address: 8002");
    }
}