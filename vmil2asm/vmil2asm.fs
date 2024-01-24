module vmil2asm.main

open System.IO
open FParsec
open vmil2asm.types
open vmil2asm.parser
open vmil2asm.codeGen

let filterOutComments instructions =
    let filter i =
        match i with
        | Code c -> Some c
        | Comment _ -> None
    instructions |> List.map filter |> List.choose id

let processFile (file:FileInfo) =
    match parseFile file.FullName with
    | Success(results, _, _) ->
        let code = filterOutComments results
        codeGenInstructions file.Name code
    | Failure(msg, _, _) -> failwith msg

let assemblyInstructionToString ai =
    match ai with
    | AssemblyInstruction a -> a    
    
let processRequest req =
    let x =
        req.inputFiles
        |> List.map processFile
        |> List.collect id
        |> List.map assemblyInstructionToString
    File.WriteAllLines(req.outputName, x)