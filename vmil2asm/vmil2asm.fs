module vmil2asm.api

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

let assemblyInstructionToString ai =
    match ai with
    | AssemblyInstruction a -> a    

let vmil2asmFile (file:FileInfo) =
    printfn $"\tProcessing {file}"
    match parseFile file.FullName with
    | Success(results, _, _) ->
        let code = filterOutComments results
        codeGenInstructions file.Name code
    | Failure(msg, _, _) -> failwith msg

let vmil2asmString name input =
    match parseString input with
    | Success(results, _, _) ->
        let code = filterOutComments results
        codeGenInstructions name code
        |> List.map assemblyInstructionToString
    | Failure(msg, _, _) -> failwith msg

let vmil2asmStrings (inputs: StringRequest list) =
    inputs
    |> List.map (fun req -> vmil2asmString req.name req.input)
    |> List.collect id
    
let vmil2asmRequest req =
    let x =
        req.inputFiles
        |> List.map vmil2asmFile
        |> List.collect id
        |> List.map assemblyInstructionToString
    File.WriteAllLines(req.outputName, x)