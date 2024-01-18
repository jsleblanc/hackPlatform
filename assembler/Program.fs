
open System
open Argu
open assembler.parsers

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
    
    let a = parseAssemblyFile @"/Users/josephleblanc/Documents/Code/nand2tetris/projects/06/max/Max.asm" System.Text.Encoding.UTF8
    let b = parseAssemblyFile @"/Users/josephleblanc/Documents/Code/nand2tetris/projects/06/pong/Pong.asm" System.Text.Encoding.UTF8
// For more information see https://aka.ms/fsharp-console-apps
    printfn "Hello from F#"
    0