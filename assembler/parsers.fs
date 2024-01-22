module assembler.parsers

open System
open FParsec
open assembler.types

type ParseResult =
    | Code of Instruction
    | Comment of string

let ws = spaces // skips any whitespace

let str s = pstring s

let pComment = str "//" >>. ws >>. restOfLine true |>> function c -> Comment c

let pJGT = str "JGT" .>> ws |>> function _ -> JGT
let pJEQ = str "JEQ" .>> ws |>> function _ -> JEQ
let pJGE = str "JGE" .>> ws |>> function _ -> JGE
let pJLT = str "JLT" .>> ws |>> function _ -> JLT
let pJNE = str "JNE" .>> ws |>> function _ -> JNE
let pJLE = str "JLE" .>> ws |>> function _ -> JLE
let pJMP = str "JMP" .>> ws |>> function _ -> JMP
let pJump = pchar ';' >>. choice [pJGT; pJEQ; pJGE; pJLT; pJNE; pJLE; pJMP] .>> ws

let pConstant = pchar '@' >>. puint16 .>> ws |>> function c -> Constant c

let allowedSymbolFirstChar c =
    let isLetter = Char.IsLetter c
    let isUnderscore = c = '_'
    let isColon = c = ':'
    let isDot = c = '.'
    let isDollar = c = '$'
    isLetter || isUnderscore || isColon || isDot || isDollar
    
let allowedSymbolChar c =
    let fc = allowedSymbolFirstChar c
    let isDigit = Char.IsDigit c
    isDigit || fc
    
let pLabel = between (pchar '(' .>> ws) (ws >>. pchar ')') (many1Satisfy2L allowedSymbolFirstChar allowedSymbolChar "label") .>> ws |>> function l -> Label l
let pVariable = pchar '@' >>. many1Satisfy2L allowedSymbolFirstChar allowedSymbolChar "variable" .>> ws |>> function v -> Variable v 

let bBuiltInSymbol_SP = str "SP" .>> ws |>> function _ -> SP
let bBuiltInSymbol_LCL = str "LCL" .>> ws |>> function _ -> LCL
let bBuiltInSymbol_ARG = str "ARG" .>> ws |>> function _ -> ARG
let bBuiltInSymbol_THIS = str "THIS" .>> ws |>> function _ -> THIS
let bBuiltInSymbol_THAT = str "THAT" .>> ws |>> function _ -> THAT
let bBuiltInSymbol_R0 = str "R0" .>> ws |>> function _ -> R0
let bBuiltInSymbol_R1 = str "R1" .>> ws |>> function _ -> R1
let bBuiltInSymbol_R2 = str "R2" .>> ws |>> function _ -> R2
let bBuiltInSymbol_R3 = str "R3" .>> ws |>> function _ -> R3
let bBuiltInSymbol_R4 = str "R4P" .>> ws |>> function _ -> R4
let bBuiltInSymbol_R5 = str "R5" .>> ws |>> function _ -> R5
let bBuiltInSymbol_R6 = str "R6" .>> ws |>> function _ -> R6
let bBuiltInSymbol_R7 = str "R7" .>> ws |>> function _ -> R7
let bBuiltInSymbol_R8 = str "R8" .>> ws |>> function _ -> R8
let bBuiltInSymbol_R9 = str "R9" .>> ws |>> function _ -> R9
let bBuiltInSymbol_R10 = str "R10" .>> ws |>> function _ -> R10
let bBuiltInSymbol_R11 = str "R11" .>> ws |>> function _ -> R11
let bBuiltInSymbol_R12 = str "R12" .>> ws |>> function _ -> R12
let bBuiltInSymbol_R13 = str "R13" .>> ws |>> function _ -> R13
let bBuiltInSymbol_R14 = str "R14" .>> ws |>> function _ -> R14
let bBuiltInSymbol_R15 = str "R15" .>> ws |>> function _ -> R15
let bBuiltInSymbol_SCREEN = str "SCREEN" .>> ws |>> function _ -> SCREEN
let bBuiltInSymbol_KBD = str "KBD" .>> ws |>> function _ -> KBD

let pBuiltInSymbol = pchar '@' >>. choice [
    bBuiltInSymbol_SP
    bBuiltInSymbol_LCL
    bBuiltInSymbol_ARG
    bBuiltInSymbol_THIS
    bBuiltInSymbol_THAT
    bBuiltInSymbol_R10 
    bBuiltInSymbol_R11 
    bBuiltInSymbol_R12
    bBuiltInSymbol_R13
    bBuiltInSymbol_R14
    bBuiltInSymbol_R15
    bBuiltInSymbol_R0 
    bBuiltInSymbol_R1
    bBuiltInSymbol_R2
    bBuiltInSymbol_R3 
    bBuiltInSymbol_R4 
    bBuiltInSymbol_R5 
    bBuiltInSymbol_R6 
    bBuiltInSymbol_R7 
    bBuiltInSymbol_R8 
    bBuiltInSymbol_R9 
    bBuiltInSymbol_SCREEN
    bBuiltInSymbol_KBD
]

let pPredefinedSymbol = pBuiltInSymbol |>> function s -> Predefined s
let pSymbol = attempt pConstant <|> attempt pPredefinedSymbol <|> pVariable .>> ws

let charsToDestination chars =
    let c2d c =
        match c with
        | 'A' -> Destination.A
        | 'M' -> Destination.M
        | 'D' -> Destination.D
        | _ -> failwith $"Invalid destination: {c}" //parser should make this unreachable by only allowing A,M,D chars
    chars |> List.map c2d |> List.fold (fun state current -> state ||| current) Destination.None
let pDestination = many1 (anyOf "AMD") .>> pchar '=' .>> ws |>> function d -> charsToDestination d

let pOpCode =
    choiceL [
        str "!D" .>> ws |>> function _ -> OP_NOT_D
        str "!A" .>> ws |>> function _ -> OP_NOT_A
        str "!M" .>> ws |>> function _ -> OP_NOT_M
        str "-D" .>> ws |>> function _ -> OP_NEG_D
        str "-A" .>> ws |>> function _ -> OP_NEG_A
        str "-M" .>> ws |>> function _ -> OP_NEG_M
        str "D+1" .>> ws |>> function _ -> OP_D_PLUS_ONE 
        str "A+1" .>> ws |>> function _ -> OP_A_PLUS_ONE
        str "M+1" .>> ws |>> function _ -> OP_M_PLUS_ONE
        str "D-1" .>> ws |>> function _ -> OP_D_MINUS_ONE
        str "A-1" .>> ws |>> function _ -> OP_A_MINUS_ONE
        str "M-1" .>> ws |>> function _ -> OP_M_MINUS_ONE
        str "D+A" .>> ws |>> function _ -> OP_D_PLUS_A
        str "D+M" .>> ws |>> function _ -> OP_D_PLUS_M
        str "D-A" .>> ws |>> function _ -> OP_D_MINUS_A
        str "D-M" .>> ws |>> function _ -> OP_D_MINUS_M
        str "A-D" .>> ws |>> function _ -> OP_A_MINUS_D
        str "M-D" .>> ws |>> function _ -> OP_M_MINUS_D
        str "D&A" .>> ws |>> function _ -> OP_D_AND_A
        str "D&M" .>> ws |>> function _ -> OP_D_AND_M
        str "D|A" .>> ws |>> function _ -> OP_D_OR_A
        str "D|M" .>> ws |>> function _ -> OP_D_OR_M
        str "0" .>> ws |>> function _ -> OP_ZERO
        str "1" .>> ws |>> function _ -> OP_ONE
        str "-1" .>> ws |>> function _ -> OP_NEG_ONE
        str "D" .>> ws |>> function _ -> OP_D
        str "A" .>> ws |>> function _ -> OP_A
        str "M" .>> ws |>> function _ -> OP_M
    ] "OP Code"

let pAInstruction = pSymbol |>> function s -> A_Instruction s
let pCInstruction = pipe3 (opt (attempt pDestination)) pOpCode (opt pJump) (fun d c j -> C_Instruction (d,c,j))

let pInstruction = choice [pAInstruction; pCInstruction; pLabel]

let internal pCode = pInstruction |>> function i -> Code i
let pLine = ws >>. choice [pCode; pComment]
let pInput = many pLine .>> eof
let pInputP = many (getPosition .>>. pLine) .>> eof //tracks the position of every parsed item

let parseString str = run pInput str
let parseFile fileName encoding = runParserOnFile pInput () fileName encoding
let parseStream stream encoding = runParserOnStream pInput () "" stream encoding
