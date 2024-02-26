using hackemu;
using FluentAssertions;

namespace hackemuTests.E2E.ConvertToBin;

public class ConvertToBinTests
{
    [Fact]
    public void ConvertToBinProgramTest_Zero()
    {
        var binaryCode =
            CompilerWorkflow.CompileFiles(new[] { new FileInfo("E2E/ConvertToBin/Main.jack") });
        
        var hackComputer = new HackComputer(binaryCode);
        hackComputer.SetMemory(8000, 0);
        hackComputer.ComputeCycles(1_000_000);
        
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
        hackComputer.ComputeCycles(1_000_000);
        
        for (ushort x = 8001; x <= 8015; x++)
        {
            hackComputer.Memory(x).Should().Be(1, $"Memory address: {x}");
        }
        hackComputer.Memory(8016).Should().Be(0, "Memory address: 8016");
    }   
    
    [Fact]
    public void ConvertToBinProgramTest_0x5555()
    {
        var binaryCode =
            CompilerWorkflow.CompileFiles(new[] { new FileInfo("E2E/ConvertToBin/Main.jack") });
        
        var hackComputer = new HackComputer(binaryCode);
        hackComputer.SetMemory(8000, 0x5555);
        hackComputer.ComputeCycles(1_000_000);
        
        hackComputer.Memory(8001).Should().Be(1, "Memory address: 8001");
        hackComputer.Memory(8002).Should().Be(0, "Memory address: 8002");
        hackComputer.Memory(8003).Should().Be(1, "Memory address: 8003");
        hackComputer.Memory(8004).Should().Be(0, "Memory address: 8004");
        hackComputer.Memory(8005).Should().Be(1, "Memory address: 8005");
        hackComputer.Memory(8006).Should().Be(0, "Memory address: 8006");
        hackComputer.Memory(8007).Should().Be(1, "Memory address: 8007");
        hackComputer.Memory(8008).Should().Be(0, "Memory address: 8008");
        hackComputer.Memory(8009).Should().Be(1, "Memory address: 8009");
        hackComputer.Memory(8010).Should().Be(0, "Memory address: 8010");
        hackComputer.Memory(8011).Should().Be(1, "Memory address: 8011");
        hackComputer.Memory(8012).Should().Be(0, "Memory address: 8012");
        hackComputer.Memory(8013).Should().Be(1, "Memory address: 8013");
        hackComputer.Memory(8014).Should().Be(0, "Memory address: 8014");
        hackComputer.Memory(8015).Should().Be(1, "Memory address: 8015");
        hackComputer.Memory(8016).Should().Be(0, "Memory address: 8016");
    }        
}