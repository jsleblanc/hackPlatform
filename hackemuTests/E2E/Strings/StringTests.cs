using System.Text;
using FluentAssertions;
using hackemu;

namespace hackemuTests.E2E.Strings;

public class StringTests
{
    [Fact]
    public void StringConstantTest()
    {
        var binaryCode =
            CompilerWorkflow.CompileFiles(new[] { new FileInfo("E2E/Strings/Main.jack") });
        
        var hackComputer = new HackComputer(binaryCode);
        hackComputer.ComputeCycles(3_000_000);

        ValidateString("Hello, world!", 8001, hackComputer);
        ValidateString("You didn't say the magic word", 8002, hackComputer);
    }

    private static void ValidateString(string expected, ushort address, HackComputer hc)
    {
        var expectedBytes = Encoding.ASCII.GetBytes(expected);
        var allocatedLength = (short)expectedBytes.Length;

        hc.Memory(address).Should().NotBe(0);
        var baseAddress = (ushort)hc.Memory(address);

        hc.Memory(baseAddress).Should().Be(allocatedLength); //one field is allocated capacity
        hc.Memory((ushort)(baseAddress + 2)).Should().Be(allocatedLength); //the other is used capacity (ie: string length)
        
        var strBaseAddress = (ushort)hc.Memory((ushort)(baseAddress + 1));

        for (var index = 0; index < expectedBytes.Length; index++)
        {
            var strAddress = strBaseAddress + index;
            hc.Memory((ushort)strAddress).Should().Be(expectedBytes[index],
                $"string base address {strBaseAddress} offset {index} ({strAddress})");
        }
    }
}