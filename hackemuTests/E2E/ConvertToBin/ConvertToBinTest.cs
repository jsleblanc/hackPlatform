using hackemu;

namespace hackemuTests.E2E.ConvertToBin;

public class ConvertToBinTest
{
    [Fact]
    public void ConvertToBinProgramTest_Zero()
    {
        var binaryCode =
            CompilerWorkflow.CompileFiles(new[] { new FileInfo("E2E/ConvertToBin/Main.jack") });
        
        var hackComputer = new HackComputer(binaryCode);
        hackComputer.SetMemory(8000, 0);
        hackComputer.ComputeCycles(10_000);
        
        for (short x = 8001; x <= 8016; x++)
        {
            Assert.Equal(0, hackComputer.Memory(x));
        }
    }
}