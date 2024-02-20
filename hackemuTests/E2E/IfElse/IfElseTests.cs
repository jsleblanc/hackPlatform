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
        hackComputer.ComputeCycles(1000);

        Assert.Equal(1, hackComputer.Memory(8001));
        Assert.Equal(2, hackComputer.Memory(8002));
    }
}