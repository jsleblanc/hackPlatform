open System
open Argu

open jackc.api
open jackc.util
open jackc.validation

type Arguments =
    | [<MainCommand; ExactlyOnce>] InputPath of path: string
    | [<Unique; AltCommandLine("-o");>] OutputPath of outputPath: string option
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | InputPath _ -> "Path to a single .jack file or path to a directory containing .jack files to compile"
            | OutputPath _ -> "Specify the path to write compiled .vm files to, instead of the same folder as the input .jack files"
            
[<EntryPoint>]
let main argv =
    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
    let parser = ArgumentParser.Create<Arguments>(errorHandler = errorHandler)
    let results = parser.ParseCommandLine argv
    
    if results.IsUsageRequested then
        printfn $"%s{parser.PrintUsage()}"
        exit 0
        
    let inputPath = results.GetResult InputPath
    let outputPathOpt = results.GetResult OutputPath
    printfn "Jack to Virtual Machine IL Compiler"
    printfn $"Processing {inputPath}"
    let files = findInputFiles inputPath
    let outputPath = computeOutputPath inputPath outputPathOpt
    match files, outputPath with
    | [], None ->
        printfn $"Error: specified path \"{inputPath}\" is not a .jack file or a directory containing .jack files"
        -1
    | xs, Some output ->
        match compileFiles xs with
        | OK compiledCode ->
            writeCompiledCodeToDisk output compiledCode
            0
        | Invalid errors ->
            printfn $"Failed to compile due to errors ({errors.Length}):"
            printErrors errors
            -1
    | _ ->
        printfn $"Could not determine an output path from \"{inputPath}\""
        -1
            