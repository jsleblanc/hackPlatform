module assembler.parsers

open System
open FParsec
open assembler.types

let ws = spaces // skips any whitespace

let str s = pstring s
let stringReturn_ws s t = stringReturn s t .>> ws

/// Comment parsing help from https://stackoverflow.com/a/24073060/125882
/// Type abbreviation for parsers without user state.
type Parser<'a> = Parser<'a, Unit>

/// Skips C-style multiline comment /*...*/ with arbitrary nesting depth.
let (pCommentImpl : Parser<_>), pCommentRef = createParserForwardedToRef ()

/// Skips any character not beginning of comment end marker */.
let skipCommentChar : Parser<_> = 
    notFollowedBy (skipString "*/") >>. skipAnyChar

/// Skips anx mix of nested comments or comment characters.
let commentContent : Parser<_> =
    skipMany (choice [ pCommentImpl; skipCommentChar ])

// Skips C-style multiline comment /*...*/ with arbitrary nesting depth.
do pCommentRef :=
       choiceL [                  
           between (skipString "/*") (skipString "*/") commentContent
           str "//" .>> ws >>. restOfLine true |>> function _ -> ()
       ] "comment" .>> ws

let pComment = optional (many pCommentImpl)

let pJGT = stringReturn "JGT" JGT
let pJEQ = stringReturn "JEQ" JEQ
let pJGE = stringReturn "JGE" JGE
let pJLT = stringReturn "JLT" JLT
let pJNE = stringReturn "JNE" JNE
let pJLE = stringReturn "JLE" JLE
let pJMP = stringReturn "JMP" JMP
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

let bBuiltInSymbol_SP = stringReturn_ws "SP" SP
let bBuiltInSymbol_LCL = stringReturn_ws "LCL" LCL
let bBuiltInSymbol_ARG = stringReturn_ws "ARG" ARG
let bBuiltInSymbol_THIS = stringReturn_ws "THIS" THIS
let bBuiltInSymbol_THAT = stringReturn_ws "THAT" THAT
let bBuiltInSymbol_R0 = stringReturn_ws "R0" R0
let bBuiltInSymbol_R1 = stringReturn_ws "R1" R1
let bBuiltInSymbol_R2 = stringReturn_ws "R2" R2
let bBuiltInSymbol_R3 = stringReturn_ws "R3" R3
let bBuiltInSymbol_R4 = stringReturn_ws "R4" R4
let bBuiltInSymbol_R5 = stringReturn_ws "R5" R5
let bBuiltInSymbol_R6 = stringReturn_ws "R6" R6
let bBuiltInSymbol_R7 = stringReturn_ws "R7" R7
let bBuiltInSymbol_R8 = stringReturn_ws "R8" R8
let bBuiltInSymbol_R9 = stringReturn_ws "R9" R9
let bBuiltInSymbol_R10 = stringReturn_ws "R10" R10
let bBuiltInSymbol_R11 = stringReturn_ws "R11" R11
let bBuiltInSymbol_R12 = stringReturn_ws "R12" R12
let bBuiltInSymbol_R13 = stringReturn_ws "R13" R13
let bBuiltInSymbol_R14 = stringReturn_ws "R14" R14
let bBuiltInSymbol_R15 = stringReturn_ws "R15" R15
let bBuiltInSymbol_SCREEN = stringReturn_ws "SCREEN" SCREEN
let bBuiltInSymbol_KBD = stringReturn_ws "KBD" KBD
let bBuiltInSymbol_RET = stringReturn_ws "RET" RET
let bBuiltInSymbol_FRAME = stringReturn_ws "FRAME" FRAME

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
    bBuiltInSymbol_RET
    bBuiltInSymbol_FRAME
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

let pInstruction = choice [pAInstruction; pCInstruction; pLabel] .>> ws

let pLine = ws >>. pComment >>. pInstruction
let pInput = many pLine .>> eof
let pInputP = many (getPosition .>>. pLine) .>> eof //tracks the position of every parsed item

let parseString str = run pInput str
let parseFile fileName encoding = runParserOnFile pInput () fileName encoding
let parseStream stream encoding = runParserOnStream pInput () "" stream encoding
