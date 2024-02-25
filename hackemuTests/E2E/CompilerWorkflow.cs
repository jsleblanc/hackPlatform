using System.Text;
using Microsoft.FSharp.Collections;
using vmil2asm;

namespace hackemuTests.E2E;

public static class CompilerWorkflow
{
    private static IEnumerable<FileInfo> SystemFiles() => new[]
    {
        new FileInfo("E2E/OSVM/Sys.vm"),
        new FileInfo("E2E/OSVM/Memory.vm"),
        new FileInfo("E2E/OSVM/Math.vm"),
        //new FileInfo("E2E/OSVM/Output.vm"),
        new FileInfo("E2E/OSVM/Array.vm"),
        new FileInfo("E2E/OSVM/String.vm"),
        //new FileInfo("E2E/OSVM/Screen.vm"),
        //new FileInfo("E2E/OSVM/Keyboard.vm")
    };

    private static IEnumerable<Tuple<string, string>> ReadSystemFiles() =>
        SystemFiles().Select(f => new Tuple<string, string>(f.Name, File.ReadAllText(f.FullName)));
    
    public static IReadOnlyList<string> CompileFiles(IEnumerable<FileInfo> files)
    {
        var compileResult = jackc.api.compileFiles(ListModule.OfSeq(files));
        var errors = jackc.validation.errors(compileResult);
        if (!errors.IsEmpty)
        {
            throw new Exception(errors.Aggregate(string.Empty, (acc, err) => acc + err.message + "; "));
        }

        var vmilCode = jackc.validation.choose(compileResult).Value.ToList();
        
        var systemCode = ReadSystemFiles();
        var userCode = vmilCode.Select(c => new Tuple<string, string>(c.name, c.code));
        var allCode = userCode.Concat(systemCode).ToList();

        var stringReq = new types.ProcessStringsRequest(ListModule.OfSeq(allCode), true);
        var assemblyInstructions = api.vmil2asmStrings(stringReq);

        var assemblyCode = assemblyInstructions.Aggregate(new StringBuilder(),
            (builder, s) => builder.AppendLine(s), sb => sb.ToString());

        var binaryCode = assembler.api.assemble(assemblyCode);
        return binaryCode.instructions;
    }
}