
open System
open Argu
open FParsec.CharParsers
open assembler.parsers
open assembler.translator
open assembler.util

open System.IO
type Arguments =
    | [<MainCommand; ExactlyOnce>] AssemblyFile of file:string
    | [<Unique; AltCommandLine("-d")>] DumpSymbolTable
    | [<Unique; AltCommandLine("-o")>] OutputFile of outputFile:string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | AssemblyFile _ -> "Assembly file to process"
            | DumpSymbolTable -> "Writes out the symbol address table to a file to assist debugging"
            | OutputFile _ -> "Specify the name of the output file instead of the default <inputFile>.hack"

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(errorHandler = errorHandler)
    let results = parser.ParseCommandLine argv
    
    if results.IsUsageRequested then
        printfn $"%s{parser.PrintUsage()}"
        exit 0
    
    let fileName = results.GetResult AssemblyFile
    let dumpSymbolTableArg = results.TryGetResult DumpSymbolTable
    let outputFileNameArg = results.TryGetResult OutputFile
    
    let file = FileInfo(fileName)
    if file.Exists = false then
        printfn $"Error: specified file \"{fileName}\" does not exist"
        exit -1
    
    let outputFileName =
        match outputFileNameArg with
        | Some name -> name
        | None -> Path.ChangeExtension(file.FullName, ".hack")
    
    let parsedAssemblyFileResult = parseStream (file.OpenRead()) System.Text.Encoding.Default
    let retCode =
        match parsedAssemblyFileResult with
        | Success (results, _, _) ->
            let translated = translate results
            File.WriteAllLines(outputFileName, translated.instructions)
            printfn $"Assembled program written to \"{outputFileName}\""
            match dumpSymbolTableArg with
            | Some _ -> 
                let symbolOutputFileName = Path.ChangeExtension(file.FullName, ".hack.csv")
                dumpSymbolsToDisk translated.symbolTable symbolOutputFileName
                printfn $"Symbol table written to \"{symbolOutputFileName}\""
            | _ -> ()
            0
        | Failure(msg, error, _) ->
            printfn $"Error parsing assembly program: {msg}"
            -1
    
    retCode