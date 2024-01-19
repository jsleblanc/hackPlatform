module assembler.translator

open System.Collections.Generic
open assembler.types

[<Literal>]
let VARIABLE_BASE_ADDRESS = 0x10
let internal u = uint16

let builtInSymbolToString s =
    match s with
    | SP -> "SP"
    | LCL -> "LCL"
    | ARG -> "ARG"
    | THIS -> "THIS"
    | THAT -> "THAT"
    | R0 -> "R0"
    | R1 -> "R1"
    | R2 -> "R2"
    | R3 -> "R3"
    | R4 -> "R4"
    | R5 -> "R5"
    | R6 -> "R6"
    | R7 -> "R7"
    | R8 -> "R8"
    | R9 -> "R9"
    | R10 -> "R10"
    | R11 -> "R11"
    | R12 -> "R12"
    | R13 -> "R13"
    | R14 -> "R14"
    | R15 -> "R15"
    | SCREEN -> "SCREEN"
    | KBD -> "KBD"

let seedSymbolMap =
    let f = builtInSymbolToString
    Map [
        (f SP), u 0
        (f LCL), u 1
        (f ARG), u 2
        (f THIS), u 3
        (f THAT), u 4
        (f R0), u 0
        (f R1), u 1
        (f R2), u 2
        (f R3), u 3
        (f R4), u 4
        (f R5), u 5
        (f R6), u 6
        (f R7), u 7
        (f R8), u 8
        (f R9), u 9
        (f R10), u 0xA
        (f R11), u 0xB
        (f R12), u 0xC
        (f R13), u 0xD
        (f R14), u 0xE
        (f R15), u 0xF
        (f SCREEN), u 0x4000
        (f KBD), u 0x6000
    ]
    
let buildSymbolTable instructions =
    let mutable pc = u 0
    let mutable vc = u 0x10
    let table = Dictionary<string, uint16>()
    let processInstruction i =
        match i with
        | Label l ->
            if table.ContainsKey(l) = false then
                table.Add(l, pc)
        | _ -> pc <- pc + (u 1)
        match i with
        | A_Instruction (Variable v) ->
            if table.ContainsKey(v) = false then
                table.Add(v, vc)
                vc <- vc + (u 1)
        | _ -> ()
    instructions |> List.iter processInstruction
    let seedItems = seedSymbolMap |> Map.toList
    let mappedSymbols = table |> Seq.map (|KeyValue|) |> Seq.toList
    seedItems @ mappedSymbols |> Map.ofList

let translate (instructions: Instruction list) =
    let symbolTable = buildSymbolTable instructions
    {
        instructions = ["todo"]
        symbolTable = symbolTable
    }