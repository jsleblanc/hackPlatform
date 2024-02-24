module assembler.translator

open System
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
    | FRAME -> "FRAME"
    | RET -> "RET"

let jump2Bin j =
    let x = 
        match j with
        | JGT -> 0b001
        | JEQ -> 0b010
        | JGE -> 0b011
        | JLT -> 0b100
        | JNE -> 0b101
        | JLE -> 0b110
        | JMP -> 0b111
    u x

let comp2bin c =
    //a,c1,c2,c3,c4,c5,c6
    let x = 
        match c with
        | OP_ZERO -> 0b0_101010
        | OP_ONE -> 0b0_111111
        | OP_NEG_ONE -> 0b0_111010
        | OP_D  -> 0b0_001100
        | OP_A  -> 0b0_110000
        | OP_M  -> 0b1_110000
        | OP_NOT_D -> 0b0_001101
        | OP_NOT_A -> 0b0_110001
        | OP_NOT_M -> 0b1_110001
        | OP_NEG_D -> 0b0_001111
        | OP_NEG_A -> 0b0_110011    
        | OP_NEG_M -> 0b1_110011
        | OP_D_PLUS_ONE -> 0b0_011111
        | OP_A_PLUS_ONE -> 0b0_110111
        | OP_M_PLUS_ONE -> 0b1_110111
        | OP_D_MINUS_ONE -> 0b0_001110
        | OP_A_MINUS_ONE -> 0b0_110010
        | OP_M_MINUS_ONE -> 0b1_110010
        | OP_D_PLUS_A -> 0b0_000010
        | OP_D_PLUS_M -> 0b1_000010
        | OP_D_MINUS_A -> 0b0_010011
        | OP_D_MINUS_M -> 0b1_010011
        | OP_A_MINUS_D -> 0b0_000111
        | OP_M_MINUS_D -> 0b1_000111
        | OP_D_AND_A -> 0b0_000000
        | OP_D_AND_M -> 0b1_000000
        | OP_D_OR_A -> 0b0_010101
        | OP_D_OR_M -> 0b1_010101
    u x
    
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
        (f FRAME), u 0xE
        (f RET), u 0xF
        (f SCREEN), u 0x4000
        (f KBD), u 0x6000
    ]

//Anything larger than this value will be interpreted as a computation instruction and not a program address
[<Literal>]
let SYMBOL_LABEL_LIMIT = 0b1000000000000000us

let checkSymbolsObeyLimit (t:Map<string, uint16>) =
    t |> Map.forall (fun _ v -> v < SYMBOL_LABEL_LIMIT)
    
let buildSymbolTable instructions =
    let mutable pc = u 0
    let mutable vc = u VARIABLE_BASE_ADDRESS
    let table = Dictionary<string, uint16>()
    let processLabels i =
        match i with
        | Label l ->
            if table.ContainsKey(l) = false then
                table.Add(l, pc)
        | _ -> pc <- pc + (u 1)
    let processVariables i =
        match i with
        | A_Instruction (Variable v) ->
            if table.ContainsKey(v) = false then
                table.Add(v, vc)
                vc <- vc + (u 1)
        | _ -> ()
    instructions |> List.iter processLabels
    instructions |> List.iter processVariables
    let seedItems = seedSymbolMap |> Map.toList
    let mappedSymbols = table |> Seq.map (|KeyValue|) |> Seq.toList
    seedItems @ mappedSymbols |> Map.ofList

let i2b (i:uint16) = Convert.ToString(int i, 2).PadLeft(16, '0')

let jmpOrZero j =
    match j with
    | Some jmp -> jump2Bin jmp
    | None -> u 0

let destOrZero (d:Destination option) =
    match d with
    | Some dst -> u (int dst)
    | None -> u 0

let translateInstruction (t:Map<string, uint16>) i =
    match i with
    | A_Instruction a ->
        match a with
        | Predefined p -> Some (i2b t[builtInSymbolToString p])
        | Variable v -> Some (i2b t[v])
        | Constant c -> Some (i2b c)
    | C_Instruction (d, comp, j) ->
        let compBin = (comp2bin comp) <<< 6
        let dstBin = (destOrZero d) <<< 3
        let jumpBin = jmpOrZero j
        let bin = (u 0b111_0000000000000) ||| compBin ||| dstBin ||| jumpBin
        Some (i2b bin)
    | Label _ -> None

let translateInstructionBuiltinSymbolTable i =
    translateInstruction seedSymbolMap i

let translate (instructions: Instruction list) =
    let symbolTable = buildSymbolTable instructions
    let symbolsValid = checkSymbolsObeyLimit symbolTable
    if symbolsValid = false then failwith "The ROM address of one or more symbols exceeds the valid range for the HACK computer platform. Values above a certain size will be interpreted as computation instructions instead of program addresses, resulting in program crashes."
    {
        instructions = instructions |> List.map (translateInstruction symbolTable) |> List.choose id
        symbolTable = symbolTable
    }