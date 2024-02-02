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

//Expressions
let pExpressionKeywordConstant =
    choiceL [
        stringReturn_ws "true" (J_Bool true)
        stringReturn_ws "false" (J_Bool false)
        stringReturn_ws "null" J_Null
        stringReturn_ws "this" J_This
    ] "expression constant" .>> ws

let pExpressionUnaryOp =
    choiceL [
        stringReturn_ws "-" J_NEG
        stringReturn_ws "~" J_NOT
    ] "unary operator" .>> ws

let pExpressionBinaryOp =
    choiceL [
        stringReturn_ws "+" J_ADD
        stringReturn_ws "-" J_SUB
        stringReturn_ws "*" J_MUL
        stringReturn_ws "/" J_DIV
        stringReturn_ws "&" J_AND
        stringReturn_ws "|" J_OR
        stringReturn_ws "<" J_LT
        stringReturn_ws "<" J_GT
        stringReturn_ws "=" J_EQ
    ] "binary operator" .>> ws
    
let pExpressionIntConstant = pint16 .>> ws |>> function i -> J_Constant_Int i
let pExpressionStringConstant = between (str "\"") (str "\"") (manySatisfy (fun c -> c <> '"')) .>> ws |>> function s -> J_Constant_String s

let pExpressionImpl, pExpressionRef = createParserForwardedToRef()
let pExpressionTermImpl, pExpressionTermRef = createParserForwardedToRef()

do pExpressionTermRef :=
    choiceL [
        pExpressionIntConstant
        pExpressionStringConstant
        pExpressionKeywordConstant |>> function k -> J_Constant_Keyword k
        pVarName .>>. (between (str_ws "[") (str_ws "]") pExpressionImpl) .>> ws |>> function n,e -> J_ArrayIndex (n,e)
        pVarName |>> function v -> J_Variable v
        pExpressionUnaryOp |>> function o -> J_UnaryOp o
    ] "expression term" .>> ws

do pExpressionRef := pExpressionTermImpl .>>. many (pExpressionBinaryOp .>>. pExpressionTermImpl) .>> ws |>> function t,o -> J_Expression (t, o)

let pExpressionList = between (str_ws "(") (str_ws ")") (sepBy1 pExpressionImpl (str_ws ",")) .>> ws




let pExpression = pExpressionImpl


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