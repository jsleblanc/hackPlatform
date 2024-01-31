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

let pCommentSingleLine = str "//" >>. ws >>. restOfLine true 
let pCommentMultiLine = ((str "/*") <|> (str "/**")) >>. charsTillString "*/" true Int32.MaxValue .>> ws
let pComment = choiceL [pCommentSingleLine; pCommentMultiLine] "comment" |>> function _ -> Comment

let isIdStart c = isAsciiLetter c || c = '_'
let isIdContinue c = isAsciiLetter c || isDigit c || c = '_'
let pIdentifier = identifier (IdentifierOptions(isAsciiIdStart = isIdStart, isAsciiIdContinue = isIdContinue)) .>> ws

let pClassName = pIdentifier
let pSubroutineName = pIdentifier
let pVarName = pIdentifier

let pType =
    choiceL [
        str_ws "int"
        str_ws "char"
        str_ws "boolean"
        pClassName
    ] "type"

let pClassVariableDeclaration = ((str "static") <|> (str "field")) .>> ws >>. pType .>>. (sepBy1 pVarName (str_ws ",")) .>>. str ";" .>> ws 
//let pClassVariableDeclaration = pType .>>. (sepBy1 pVarName (str ",")) >>. str ";" .>> ws 
    












let pppppp s = run pClassVariableDeclaration s


let pInput = many pComment .>> eof

let parseString str = run pInput str