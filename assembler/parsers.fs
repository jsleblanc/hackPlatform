module assembler.parsers

open System
open FParsec
open assembler.types

let ws = spaces // skips any whitespace

let str s = pstring s
let str_ws s = pstring s >>. ws

let pComment = ws >>. str_ws "//" >>. skipRestOfLine true |>> function _ -> Option<Instruction>.None

let pSpacing   = // literal translation:
                 //  skipManyChars (pSpace <|> pComment)
                 // more efficient:
                     skipSepBy spaces pComment
let pJGT = str "JGT" >>. pSpacing |>> function _ -> JGT
let pJEQ = str "JEQ" >>. pSpacing |>> function _ -> JEQ
let pJGE = str "JGE" >>. pSpacing |>> function _ -> JGE
let pJLT = str "JLT" >>. pSpacing |>> function _ -> JLT
let pJNE = str "JNE" >>. pSpacing |>> function _ -> JNE
let pJLE = str "JLE" >>. pSpacing |>> function _ -> JLE
let pJMP = str "JMP" >>. pSpacing |>> function _ -> JMP
let pJump = ws >>. pchar ';' >>. ws >>. choice [pJGT; pJEQ; pJGE; pJLT; pJNE; pJLE; pJMP]

let pConstant = pchar '@' >>. puint16 |>> function c -> Constant c

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
    
let pLabel = between (pchar '(' .>> ws) (ws >>. pchar ')') (many1Satisfy2 allowedSymbolFirstChar allowedSymbolChar) |>> function l -> Label l
let pVariable = pchar '@' >>. many1Satisfy2 allowedSymbolFirstChar allowedSymbolChar |> notEmpty |>> function v -> Variable v 

let bBuiltInSymbol_SP = str "SP" |>> function _ -> SP
let bBuiltInSymbol_LCL = str "LCL" |>> function _ -> LCL
let bBuiltInSymbol_ARG = str "ARG" |>> function _ -> ARG
let bBuiltInSymbol_THIS = str "THIS" |>> function _ -> THIS
let bBuiltInSymbol_THAT = str "THAT" |>> function _ -> THAT
let bBuiltInSymbol_R0 = str "R0" |>> function _ -> R0
let bBuiltInSymbol_R1 = str "R1" |>> function _ -> R1
let bBuiltInSymbol_R2 = str "R2" |>> function _ -> R2
let bBuiltInSymbol_R3 = str "R3" |>> function _ -> R3
let bBuiltInSymbol_R4 = str "R4P" |>> function _ -> R4
let bBuiltInSymbol_R5 = str "R5" |>> function _ -> R5
let bBuiltInSymbol_R6 = str "R6" |>> function _ -> R6
let bBuiltInSymbol_R7 = str "R7" |>> function _ -> R7
let bBuiltInSymbol_R8 = str "R8" |>> function _ -> R8
let bBuiltInSymbol_R9 = str "R9" |>> function _ -> R9
let bBuiltInSymbol_R10 = str "R10" |>> function _ -> R10
let bBuiltInSymbol_R11 = str "R11" |>> function _ -> R11
let bBuiltInSymbol_R12 = str "R12" |>> function _ -> R12
let bBuiltInSymbol_R13 = str "R13" |>> function _ -> R13
let bBuiltInSymbol_R14 = str "R14" |>> function _ -> R14
let bBuiltInSymbol_R15 = str "R15" |>> function _ -> R15
let bBuiltInSymbol_SCREEN = str "SCREEN" |>> function _ -> SCREEN
let bBuiltInSymbol_KBD = str "KBD" |>> function _ -> KBD

let pBuiltInSymbol = pchar '@' >>. choice [
    bBuiltInSymbol_SP
    bBuiltInSymbol_LCL
    bBuiltInSymbol_ARG
    bBuiltInSymbol_THIS
    bBuiltInSymbol_THAT
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
    bBuiltInSymbol_R10 
    bBuiltInSymbol_R11 
    bBuiltInSymbol_R12
    bBuiltInSymbol_R13
    bBuiltInSymbol_R14
    bBuiltInSymbol_R15
    bBuiltInSymbol_SCREEN
    bBuiltInSymbol_KBD
]

let pPredefinedSymbol = pBuiltInSymbol |>> function s -> Predefined s
let pSymbol = choice [pPredefinedSymbol; pConstant; pLabel; pVariable]

let charsToDestination chars =
    let c2d c =
        match c with
        | 'A' -> Destination.A
        | 'M' -> Destination.M
        | 'D' -> Destination.D
        | _ -> failwith $"Invalid destination: {c}"
    chars |> List.map c2d |> List.fold (fun state current -> state ||| current) Destination.None
let pDestination = many1 (anyOf "AMD") .>> pchar '=' |>> function d -> charsToDestination d

let pComputation = ws >>. many1 (anyOf "01ADM!-+&|" .>> ws) |>> function d -> Computation (String.Concat(d))

let pAInstruction = pSymbol |>> function s -> A_Instruction s
let pCInstruction = pipe3 (opt pDestination) pComputation (opt pJump) (fun d c j -> C_Instruction (d,c,j))

let pInstruction = ws >>. choice [pAInstruction; pCInstruction]

let pAssembly = ws >>. skipMany pComment >>. ws >>. many (opt pInstruction <|> pComment) .>> ws .>> eof

let parseAssemblyString str = run pAssembly str
let parseAssemblyFile fileName encoding = runParserOnFile pAssembly () fileName encoding
let parseAssemblyStream stream encoding = runParserOnStream pAssembly () "" stream encoding