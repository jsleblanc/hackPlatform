module vmil2asm.api

open System.IO
open FParsec
open vmil2asm.types
open vmil2asm.parser
open vmil2asm.codeGen

let private filterOutComments instructions =
    let filter i =
        match i with
        | Code c -> Some c
        | Comment _ -> None
    instructions |> List.map filter |> List.choose id

let private assemblyInstructionToString ai =
    match ai with
    | AssemblyInstruction a -> a    

let private internal_vmil2asmFile (file:FileInfo) =
    printfn $"\tProcessing {file}"
    match parseFile file.FullName with
    | Success(results, _, _) ->
        let code = filterOutComments results
        codeGenInstructions file.Name code
    | Failure(msg, _, _) -> failwith msg

let private internal_vmil2asmString name input =
    printfn $"\tProcessing string {name}"
    match parseString input with
    | Success(results, _, _) ->
        let code = filterOutComments results
        codeGenInstructions name code
    | Failure(msg, _, _) -> failwith msg

let vmil2asmStrings (req: ProcessStringsRequest) =
    let initCode =
        match req.initVm with
        | true -> initVm
        | false -> []    
    req.inputs
    |> List.map (fun (name,code) -> internal_vmil2asmString name code)
    |> List.collect id
    |> List.insertManyAt 0 initCode
    |> List.map assemblyInstructionToString    

let vmil2asmString name input = 
    let req = { inputs = [(name, input)]; initVm = true; }
    vmil2asmStrings req

let vmil2asmStringWithoutInitCode name input = 
    let req = { inputs = [(name, input)]; initVm = false; }
    vmil2asmStrings req
       
let vmil2asmFiles (req: ProcessFilesRequest) =
    let initCode =
        match req.initVm with
        | true -> initVm
        | false -> []
    let x =
        req.inputFiles
        |> List.map internal_vmil2asmFile
        |> List.collect id
        |> List.insertManyAt 0 initCode
        |> List.map assemblyInstructionToString        
    File.WriteAllLines(req.outputName, x)
    