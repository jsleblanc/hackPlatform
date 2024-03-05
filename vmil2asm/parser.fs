module vmil2asm.parser

open System
open FParsec
open vmil2asm.types

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

//arithmetic commands
let pA_add = stringReturn_ws "add" ADD
let pA_sub = stringReturn_ws "sub" SUB
let pA_neg = stringReturn_ws "neg" NEG
let pA_eq = stringReturn_ws "eq" EQ
let pA_GT = stringReturn_ws "gt" GT
let pA_LT = stringReturn_ws "lt" LT
let pA_and = stringReturn_ws "and" AND
let pA_or = stringReturn_ws "or" OR
let pA_not = stringReturn_ws "not" NOT

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
let pS_Argument = stringReturn_ws "argument" Argument
let pS_Local = stringReturn_ws "local" Local
let pS_Static = stringReturn_ws "static" Static
let pS_Constant = stringReturn_ws "constant" Constant
let pS_This = stringReturn_ws "this" This
let pS_That = stringReturn_ws "that" That
let pS_Pointer = stringReturn_ws "pointer" Pointer
let pS_Temp = stringReturn_ws "temp" Temp

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
    ] "command" .>> pComment

let pCode =
    choice [
        pArithmeticCommand
        pCommand        
    ]
    
let pLine = ws >>. pComment >>. pCode
let pInput = many pLine .>> eof

let pInputP = many (getPosition .>>. pLine) .>> eof

let parseString str = run pInput str
let parseFile fileName = runParserOnFile pInput () fileName System.Text.Encoding.Default
let parseStream stream = runParserOnStream pInput () "" stream System.Text.Encoding.Default