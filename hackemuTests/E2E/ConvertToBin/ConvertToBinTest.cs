using hackemu;
using FluentAssertions;

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
        
        for (ushort x = 8001; x <= 8016; x++)
        {
            hackComputer.Memory(x).Should().Be(0, $"Memory address: {x}");
        }
    }
    
    [Fact]
    public void ConvertToBinProgramTest_0x7FFF()
    {
        var binaryCode =
            CompilerWorkflow.CompileFiles(new[] { new FileInfo("E2E/ConvertToBin/Main.jack") });
        
        var hackComputer = new HackComputer(binaryCode);
        hackComputer.SetMemory(8000, short.MaxValue);
        hackComputer.ComputeCycles(10_000);
        
        for (ushort x = 8001; x <= 8016; x++)
        {
            hackComputer.Memory(x).Should().Be(1, $"Memory address: {x}");
        }
    }    
}