
open System
open Argu
open FParsec.CharParsers
open assembler.parsers
open assembler.translator
open assembler.util

open System.IO
type Arguments =
    | [<MainCommand; ExactlyOnce; Last>] AssemblyFile of file:string
    | [<Unique>] DumpSymbolTable of dumpSymbols:bool
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | AssemblyFile _ -> "Assembly file to process"
            | DumpSymbolTable _ -> "If true, writes out the symbol address table to a file to assist debugging"

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(errorHandler = errorHandler)
    let results = parser.ParseCommandLine argv
    
    if results.IsUsageRequested then
        printfn $"%s{parser.PrintUsage()}"
        exit 0
    
    let fileName = results.GetResult AssemblyFile
    let dumpSymbolTable = results.TryGetResult DumpSymbolTable
    
    let file = FileInfo(fileName)
    if file.Exists = false then
        printfn $"Error: specified file \"{fileName}\" does not exist"
        exit -1
    
    let filterInstructions pr =
        match pr with
        | Code i -> Some i
        | Comment _ -> None
    
    let parsedAssemblyFileResult = parseAssemblyStream (file.OpenRead()) System.Text.Encoding.Default
    let retCode =
        match parsedAssemblyFileResult with
        | Success (results, _, _) ->
            let code = results |> List.map filterInstructions |> List.choose id
            let translated = translate code
            let outputFileName = Path.ChangeExtension(file.FullName, ".hack")
            File.WriteAllLines(outputFileName, translated.instructions)
            printfn $"Assembled program written to \"{outputFileName}\""
            match dumpSymbolTable with
            | Some(true) -> 
                let symbolOutputFileName = Path.ChangeExtension(file.FullName, ".hack.csv")
                dumpSymbolsToDisk translated.symbolTable symbolOutputFileName
                printfn $"Symbol table written to \"{symbolOutputFileName}\""
            | _ -> ()
            0
        | Failure(msg, error, _) ->
            printfn $"Error parsing assembly program: {msg}"
            -1
    
    retCode