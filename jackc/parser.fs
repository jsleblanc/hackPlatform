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

let skipChar_ws c = skipChar c .>> ws 

let pCommentSingleLine = str "//" >>. ws >>. restOfLine true 
let pCommentMultiLine = ((str "/*") <|> (str "/**")) >>. charsTillString "*/" true Int32.MaxValue .>> ws
let pComment = choiceL [pCommentSingleLine; pCommentMultiLine] "comment" |>> function _ -> Comment

let isIdStart c = isAsciiLetter c || c = '_'
let isIdContinue c = isAsciiLetter c || isDigit c || c = '_'
let pIdentifier = identifier (IdentifierOptions(isAsciiIdStart = isIdStart, isAsciiIdContinue = isIdContinue))
let pIdentifier_ws = pIdentifier .>> ws

let pClassName = pIdentifier_ws
let pSubroutineName = pIdentifier |>> function name -> JackSubroutineName name
let pSubroutineScope = pIdentifier .>> str "." |>> function s -> JackSubroutineScope s
let pVarName = pIdentifier |>> function name -> JackVariableName name
let pVarName_ws = pIdentifier_ws |>> function name -> JackVariableName name

//Expressions
let pExpressionVariable = pVarName_ws |>> function v -> J_Variable v
let pExpressionConstantInt = pint16 .>> ws |>> function i -> J_Constant_Int i
let pExpressionConstantString = between (str "\"") (str "\"") (manySatisfy (fun c -> c <> '"')) .>> ws |>> function s -> J_Constant_String s
let pExpressionConstantBoolean = choiceL [ (stringReturn_ws "true" (J_Constant_Boolean true)); (stringReturn_ws "false" (J_Constant_Boolean false)) ] "boolean"
let pExpressionConstantNull = stringReturn_ws "null" J_Constant_Null
let pExpressionConstantThis = stringReturn_ws "this" J_Constant_This

let pExpressionImpl, pExpressionRef = createParserForwardedToRef()

let pop = skipChar_ws '(' //parser skip open parenthesis
let pcp = skipChar_ws ')' //parser skip close parenthesis
let pob = skipChar_ws '['
let pcb = skipChar_ws ']'

let pExpressionArrayIndexer = pVarName .>>. (between pob pcb pExpressionImpl) |>> function n,e -> J_Array_Index (n, e)
let pExpressionList = between pop pcp (sepBy pExpressionImpl (str_ws ","))
let pExpressionSubroutineLocalCall = pSubroutineName .>>. pExpressionList |>> function n,p -> J_Subroutine_Call (None, n, p)
let pExpressionSubroutineScopedCall = pSubroutineScope .>>. pSubroutineName .>>. pExpressionList |>> function (s,n),p -> J_Subroutine_Call (Some s, n, p)

let curry f = fun x -> fun y -> f(x,y)
let pExpressionPrimary =
    choiceL [
        pExpressionConstantInt
        pExpressionConstantString
        pExpressionConstantBoolean
        pExpressionConstantNull
        pExpressionConstantThis
        attempt pExpressionSubroutineScopedCall
        attempt pExpressionSubroutineLocalCall
        attempt pExpressionArrayIndexer
        pExpressionVariable
        between pop pcp pExpressionImpl
    ] "expression"
let pExpressionUnaryNegate = skipChar_ws '-' >>. pExpressionPrimary |>> J_NEG
let pExpressionUnaryNot = skipChar_ws '~' >>. pExpressionPrimary |>> J_NOT
let pExpressionUnary = choice [ pExpressionUnaryNegate; pExpressionUnaryNot; pExpressionPrimary ]
let pExpressionBinaryAdd = skipChar_ws '+' >>% (curry J_ADD)
let pExpressionBinarySub = skipChar_ws '-' >>% (curry J_SUB)
let pExpressionBinaryMul = skipChar_ws '*' >>% (curry J_MUL)
let pExpressionBinaryDiv = skipChar_ws '/' >>% (curry J_DIV)
let pExpressionBinaryAnd = skipChar_ws '&' >>% (curry J_AND)
let pExpressionBinaryOr = skipChar_ws '|' >>% (curry J_OR)
let pExpressionBinaryLt = skipChar_ws '<' >>% (curry J_LT)
let pExpressionBinaryGt = skipChar_ws '>' >>% (curry J_GT)
let pExpressionBinaryEq = skipChar_ws '=' >>% (curry J_EQ)

let pExpr1 = chainl1 pExpressionUnary pExpressionBinaryLt
let pExpr2 = chainl1 pExpr1 pExpressionBinaryGt
let pExpr3 = chainl1 pExpr2 pExpressionBinaryEq
let pExpr4 = chainl1 pExpr3 pExpressionBinaryAnd
let pExpr5 = chainl1 pExpr4 pExpressionBinaryOr
let pExpr6 = chainl1 pExpr5 pExpressionBinaryMul
let pExpr7 = chainl1 pExpr6 pExpressionBinaryDiv
let pExpr8 = chainl1 pExpr7 pExpressionBinaryAdd
let pExpr9 = chainl1 pExpr8 pExpressionBinarySub

do pExpressionRef := pExpr9
let pExpression = pExpressionImpl


//Statements
let pStatementImpl, pStatementRef = createParserForwardedToRef()

let poc = skipChar_ws '{'
let pcc = skipChar_ws '}'

let pStatementReturn = str_ws "return" >>. (opt pExpressionImpl) .>> str_ws ";" |>> function e -> J_Return e
let pStatementLet = (str_ws "let" >>. pExpressionImpl) .>> str_ws ";" |>> function e -> J_Let e
let pStatementWhile = (str_ws "while" >>. pExpressionImpl) .>>. (between poc pcc (many pStatementImpl)) |>> function e,s -> J_While (e,s)

let pStatementPrimary =
    choiceL [
        pStatementReturn
        pStatementLet
        pStatementWhile
    ] "statement"

do pStatementRef := pStatementPrimary

let pStatement = pStatementImpl






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
    .>>. (sepBy1 pVarName_ws (str_ws ","))
    .>> str_ws ";"
    |>> function (scope, jt),names -> J_ClassVariableDeclaration (scope, jt, names)

let pLocalVariableDeclaration =
    (str_ws "var")
    >>. pType
    .>>. (sepBy1 pVarName_ws (str_ws ","))
    .>> str_ws ";"
    |>> function jt,names -> J_LocalVariableDeclaration (jt, names)    
    
let pParameterList = between (str_ws "(") (str_ws ")") (sepBy (pType .>>. pVarName_ws) (str_ws ","))

let pSubroutineReturnType =
    choiceL [
        stringReturn_ws "void" J_Void
        pType |>> function t -> J_ReturnType t
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