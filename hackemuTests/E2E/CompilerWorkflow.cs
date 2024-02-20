using System.Text;
using Microsoft.FSharp.Collections;

namespace hackemuTests.E2E;

public static class CompilerWorkflow
{
    private static IEnumerable<FileInfo> SystemFiles() => Directory
        .EnumerateFiles("/Users/josephleblanc/Documents/Code/nand2tetris/tools/OS", "*.vm")
        .Select(f => new FileInfo(f));

    private static IEnumerable<vmil2asm.types.StringRequest> ReadSystemFiles() => 
        SystemFiles().Select(f => new vmil2asm.types.StringRequest(f.Name, File.ReadAllText(f.FullName)));
    
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
        var userCode = vmilCode.Select(c => new vmil2asm.types.StringRequest(c.name, c.code));
        var allCode = systemCode.Concat(userCode);

        var assemblyCode = vmil2asm.api.vmil2asmStrings(ListModule.OfSeq(allCode)).Aggregate(new StringBuilder(),
            (builder, s) => builder.AppendLine(s), sb => sb.ToString());

        var binaryCode = assembler.api.assemble(assemblyCode);
        return binaryCode.instructions;
    }
}