module assembler.translator

open assembler.types

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
    let u = uint16
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

let extractLabel i =
    match i with
    | A_Instruction (Label l) -> Some l
    | _ -> None
    
let extractVariable i =
    match i with
    | A_Instruction (Variable v) -> Some v
    | _ -> None

let buildSymbolTable instructions =
    let f (x, y) =
        match (x, y) with
        | Some x, y -> Some (x,y)
        | _ -> None
    //labels shouldn't increment the program counter, so this code isn't correct
    let mappedLabelSymbols = instructions |> List.indexed |> List.map (fun (i,v) -> f (extractLabel v, uint16 i)) |> List.choose id
    let mappedVariables = instructions |> List.map extractVariable |> List.choose id |> List.indexed |> List.map (fun (i, v) -> (v, uint16 (i+16)))
    let seedItems = seedSymbolMap |> Map.toList
    seedItems @ mappedLabelSymbols @ mappedVariables |> Map.ofList

let translate (instructions: Instruction list) =
    let symbolTable = buildSymbolTable instructions
    {
        instructions = ["todo"]
        symbolTable = symbolTable
    }