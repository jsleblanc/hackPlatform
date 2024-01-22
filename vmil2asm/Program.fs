
open System
open System.IO
open Argu

open vmil2asm.util
open vmil2asm.parser

type Arguments =
    | [<MainCommand; ExactlyOnce>] InputPath of path:string
    | [<Unique; AltCommandLine("-o")>] OutputFile of outputFile:string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | InputPath _ -> "Directory containing .vm files to process, or specific .vm file to process"
            | OutputFile _ -> "Specify the name/path of the output file instead of the default <inputPath>.asm"

[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(errorHandler = errorHandler)
    let results = parser.ParseCommandLine argv
    
    if results.IsUsageRequested then
        printfn $"%s{parser.PrintUsage()}"
        exit 0
                
    let inputPath = results.GetResult InputPath
    let outputFileNameArg = results.TryGetResult OutputFile
    
    let requestOpt = findInputFiles inputPath
    match requestOpt with
    | Some req ->
        let outputFileName = overrideOutputName req.outputName outputFileNameArg
        printfn $"TODO --- Generate assembly code and write to \"{outputFileName}\" ---"
        -1
    | None ->
        printfn $"Error: specified path \"{inputPath}\" is not a file or a directory containing .vm files"
        -1
