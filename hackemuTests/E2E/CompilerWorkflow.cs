using System.Text;
using Microsoft.FSharp.Collections;
using vmil2asm;

namespace hackemuTests.E2E;

public static class CompilerWorkflow
{
    private static IEnumerable<FileInfo> SystemFiles() => Directory
        .EnumerateFiles("/Users/josephleblanc/Documents/Code/nand2tetris/tools/OS", "*.vm")
        .Select(f => new FileInfo(f));

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
        var allCode = systemCode.Concat(userCode).ToList();

        var stringReq = new types.ProcessStringsRequest(ListModule.OfSeq(allCode), true);
        var assemblyCode = api.vmil2asmStrings(stringReq).Aggregate(new StringBuilder(),
            (builder, s) => builder.AppendLine(s), sb => sb.ToString());

        var binaryCode = assembler.api.assemble(assemblyCode);
        return binaryCode.instructions;
    }
}