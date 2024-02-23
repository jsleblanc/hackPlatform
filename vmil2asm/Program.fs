
open System
open Argu

open vmil2asm.api
open vmil2asm.util

type Arguments =
    | [<MainCommand; ExactlyOnce>] InputPath of path:string
    | [<Unique; AltCommandLine("-o")>] OutputFile of outputFile:string
    | [<Unique; AltCommandLine("-i"); DefaultValue(true)>] InitVm of initVm:bool
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | InputPath _ -> "Directory containing .vm files to process, or specific .vm file to process"
            | OutputFile _ -> "Specify the name/path of the output file instead of the default <inputPath>.asm"
            | InitVm _ -> "When false, generates code without the virtual machine bootstrap initialization code, useful for testing"

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
    let initVmArg =
        match results.TryGetResult InitVm with
        | Some b -> b
        | None -> true
    
    printfn "Virtual Machine IL to Hack ASM"
    printfn $"Processing {inputPath}"
    let requestOpt = findInputFiles inputPath
    match requestOpt with
    | Some req ->
        let outputFileName = overrideOutputName req.outputName outputFileNameArg
        vmil2asmFiles { req with outputName = outputFileName; initVm = initVmArg }
        printfn $"Code written to {outputFileName}"
        0
    | None ->
        printfn $"Error: specified path \"{inputPath}\" is not a file or a directory containing .vm files"
        -1
