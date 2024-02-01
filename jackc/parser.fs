module jackc.parser

open System
open FParsec
open jackc.types

type ParseResult =
    | Code of JackLang
    | Comment

let ws = spaces // skips any whitespace
let str s = pstring s
let str_ws s = pstring s .>> ws
let stringReturn_ws s t = stringReturn s t .>> ws

let pCommentSingleLine = str "//" >>. ws >>. restOfLine true 
let pCommentMultiLine = ((str "/*") <|> (str "/**")) >>. charsTillString "*/" true Int32.MaxValue .>> ws
let pComment = choiceL [pCommentSingleLine; pCommentMultiLine] "comment" |>> function _ -> Comment

let isIdStart c = isAsciiLetter c || c = '_'
let isIdContinue c = isAsciiLetter c || isDigit c || c = '_'
let pIdentifier = identifier (IdentifierOptions(isAsciiIdStart = isIdStart, isAsciiIdContinue = isIdContinue)) .>> ws

let pClassName = pIdentifier
let pSubroutineName = pIdentifier |>> function name -> JackSubroutineName name
let pVarName = pIdentifier |>> function name -> JackVariableName name

let pType =
    choiceL [
        stringReturn "int" J_Int
        stringReturn "char" J_Char
        stringReturn "boolean" J_Boolean
        pClassName |>> function name -> J_Class name
    ] "type" .>> ws

let pClassVariableDeclaration =
    ((stringReturn_ws "static" J_Static) <|> (stringReturn_ws "field" J_Field))
    .>>. pType
    .>>. (sepBy1 pVarName (str_ws ","))
    .>> str_ws ";"
    |>> function (scope, jt),names -> J_ClassVariableDeclaration (scope, jt, names)

let pLocalVariableDeclaration =
    (str_ws "var")
    >>. pType
    .>>. (sepBy1 pVarName (str_ws ","))
    .>> str_ws ";"
    |>> function jt,names -> J_LocalVariableDeclaration (jt, names)    
    
let pParameterList = between (str_ws "(") (str_ws ")") (sepBy (pType .>>. pVarName) (str_ws ","))

let pSubroutineReturnType =
    choiceL [
        stringReturn_ws "void" J_Void
        pType |>> function t -> J_Return t
    ] "return type"

let pSubroutineDeclaration =
    ((stringReturn_ws "constructor" J_Constructor) <|> (stringReturn_ws "function" J_Function) <|> (stringReturn_ws "method" J_Method))
    .>>. pSubroutineReturnType
    .>>. pSubroutineName
    .>>. pParameterList
    |>> function ((subroutineType,subroutineReturnType),subroutineName),parameters -> J_SubroutineDeclaration (subroutineType, subroutineReturnType, subroutineName, parameters, "body")









let pppppp s = run pClassVariableDeclaration s


let pInput = many pComment .>> eof

let parseString str = run pInput str