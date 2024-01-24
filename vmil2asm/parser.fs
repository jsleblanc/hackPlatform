module vmil2asm.parser

open System
open FParsec
open vmil2asm.types

type ParseResult =
    | Code of Command
    | Comment of string

let ws = spaces // skips any whitespace

let str s = pstring s

let pComment = str "//" >>. ws >>. restOfLine true |>> function c -> Comment c

//arithmetic commands
let pA_add = str "add" .>> ws |>> function _ -> ADD
let pA_sub = str "sub" .>> ws |>> function _ -> SUB
let pA_neg = str "neg" .>> ws |>> function _ -> NEG
let pA_eq = str "eq" .>> ws |>> function _ -> EQ
let pA_GT = str "gt" .>> ws |>> function _ -> GT
let pA_LT = str "lt" .>> ws |>> function _ -> LT
let pA_and = str "and" .>> ws |>> function _ -> AND
let pA_or = str "or" .>> ws |>> function _ -> OR
let pA_not = str "not" .>> ws |>> function _ -> NOT

let pArithmeticCommand =
    choiceL [
        pA_add
        pA_sub
        pA_neg
        pA_eq
        pA_GT
        pA_LT
        pA_and
        pA_or
        pA_not
    ] "arithmetic command" |>> function a -> Arithmetic a

//segments
let pS_Argument = str "argument" .>> ws |>> function _ -> Argument
let pS_Local = str "local" .>> ws |>> function _ -> Local
let pS_Static = str "static" .>> ws |>> function _ -> Static
let pS_Constant = str "constant" .>> ws |>> function _ -> Constant
let pS_This = str "this" .>> ws |>> function _ -> This
let pS_That = str "that" .>> ws |>> function _ -> That
let pS_Pointer = str "pointer" .>> ws |>> function _ -> Pointer
let pS_Temp = str "temp" .>> ws |>> function _ -> Temp

let pSegment =
    choiceL [
         pS_Argument
         pS_Local
         pS_Static
         pS_Constant
         pS_This
         pS_That
         pS_Pointer
         pS_Temp
    ] "segment"
    
let pSegmentIndex = puint16 .>> ws |>> function i -> SegmentIndex i

//commands
let pC_Push = str "push" .>> ws >>. pSegment .>>. pSegmentIndex |>> function s, si -> PUSH (s,si)
let pC_Pop = str "pop" .>> ws >>. pSegment .>>. pSegmentIndex |>> function s, si -> POP (s,si)

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

let pLabelName = many1Satisfy2L allowedSymbolFirstChar allowedSymbolChar "label"   
let pLabel = str "label" >>. ws >>. pLabelName .>> ws |>> function l -> Label l
let pGoto = str "goto" >>. ws >>. pLabelName .>> ws |>> function l -> Goto l
let pIfGoto = str "if-goto" >>. ws >>. pLabelName .>> ws |>> function l -> If_Goto l
let pBranching = choice [ pLabel; pGoto; pIfGoto ]    

let pFunctionName = many1Satisfy2L allowedSymbolFirstChar allowedSymbolChar "function"
let pFunction = str "function" >>. ws >>. pipe2 (pFunctionName .>> ws) (pint32 .>> ws) (fun n a -> Function (n, a))
let pCall = str "call" >>. ws >>. pipe2 (pFunctionName .>> ws) (pint32 .>> ws) (fun n a -> Call (n, a))
let pReturn = str "return" .>> ws |>> function _ -> Return

let pFunctions = choice [pFunction; pCall; pReturn;]

let pCommand =
    choiceL [
        pC_Push
        pC_Pop
        pBranching
        pFunctions
    ] "command"

let pCode =
    choice [
        pArithmeticCommand
        pCommand        
    ] |>> function c -> Code c
    
let pLine = ws >>. choice [pCode; pComment]

let pInput = many pLine .>> eof

let pInputP = many (getPosition .>>. pLine) .>> eof

let parseString str = run pInput str
let parseFile fileName = runParserOnFile pInput () fileName System.Text.Encoding.Default
let parseStream stream = runParserOnStream pInput () "" stream System.Text.Encoding.Default