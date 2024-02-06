module jackc.parser

open FParsec
open jackc.types

let ws = spaces // skips any whitespace
let str s = pstring s
let str_ws s = pstring s .>> ws
let stringReturn_ws s t = stringReturn s t .>> ws

let skipChar_ws c = skipChar c .>> ws 

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
let pExpressionConstantBoolean = choiceL [ (stringReturn_ws "true" (J_Constant_Boolean true)); (stringReturn_ws "false" (J_Constant_Boolean false)) ] "boolean" .>> ws
let pExpressionConstantNull = stringReturn_ws "null" J_Constant_Null .>> ws
let pExpressionConstantThis = stringReturn_ws "this" J_Constant_This .>> ws

let pExpressionImpl, pExpressionRef = createParserForwardedToRef()

let pop = skipChar_ws '(' //parser skip open parenthesis
let pcp = skipChar_ws ')' //parser skip close parenthesis
let pob = skipChar_ws '['
let pcb = skipChar_ws ']'

let pExpressionArrayIndexer = pVarName .>>. (between pob pcb pExpressionImpl) .>> ws |>> function n,e -> J_Array_Index (n, e)
let pExpressionList = between pop pcp (sepBy pExpressionImpl (str_ws ",")) .>> ws
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

let pStatementReturn = str_ws "return" >>. (opt pExpression) .>> str_ws ";" |>> function e -> J_Return e
let pStatementLet = (str_ws "let" >>. pExpression) .>> str_ws ";" |>> function e -> J_Let e
let pStatementBlock = between poc pcc (many pStatementImpl) .>> ws
let pStatementWhile = (str_ws "while" >>. pExpression) .>>. pStatementBlock |>> function e,s -> J_While (e,s)
let pStatementIfElse = (str_ws "if") >>. pExpression .>>. pStatementBlock .>>. ((str_ws "else") >>. pStatementBlock) |>> function (c,sl),esl -> J_If_Else (c,sl, esl)
let pStatementIf = (str_ws "if") >>. pExpression .>>. pStatementBlock |>> function c,sl -> J_If_Else (c,sl,[])
let pStatementDoLocalSubroutineCall = (str_ws "do") >>. pSubroutineName .>>. pExpressionList .>> (str_ws ";") |>> function n,es -> J_Do (None, n, es)
let pStatementDoScopedSubroutineCall = (str_ws "do") >>. pSubroutineScope .>>. pSubroutineName .>>. pExpressionList .>> (str_ws ";") |>> function (s,n),es -> J_Do (Some s, n, es)

let pStatementPrimary =
    choiceL [
        pStatementReturn
        pStatementLet
        pStatementWhile
        attempt pStatementIfElse
        pStatementIf
        attempt pStatementDoScopedSubroutineCall
        pStatementDoLocalSubroutineCall
    ] "statement" 

do pStatementRef := pComment >>. pStatementPrimary .>> ws .>> pComment

let pStatement = pStatementImpl




let pType =
    choiceL [
        stringReturn "int" J_Int
        stringReturn "char" J_Char
        stringReturn "boolean" J_Boolean
        pClassName |>> function name -> J_Class name
    ] "type" .>> ws

//Subroutines   
let pParameterList = between (str_ws "(") (str_ws ")") (sepBy (pType .>>. pVarName_ws) (str_ws ","))

let pSubroutineType =
    choiceL [
       stringReturn_ws "constructor" J_Constructor
       stringReturn_ws "function" J_Function
       stringReturn_ws "method" J_Method
    ] "subroutine type"

let pSubroutineReturnType =
    choiceL [
        stringReturn_ws "void" J_Void
        pType |>> function t -> J_ReturnType t
    ] "return type"

let pSubroutineVariableDeclaration =
    (str_ws "var")
    >>. pType
    .>>. (sepBy1 pVarName_ws (str_ws ","))
    .>> str_ws ";"
    |>> function jt,names -> names |> List.map (fun n -> (jt,n))

let pSubroutineBody = pComment >>. between poc pcc ((many pSubroutineVariableDeclaration) .>>. (many pStatementImpl)) .>> ws |>> function v,s -> (List.collect id v, s)

let pSubroutineDeclaration =
    pComment >>. pipe5 pSubroutineType pSubroutineReturnType pSubroutineName pParameterList pSubroutineBody (fun a b c d (vars,statements) -> {subType = a; returnType = b; name = c; parameters = d; variables = vars; body = statements; })
            
//Classes
let pClassVariableDeclaration =
    ((stringReturn_ws "static" J_Static) <|> (stringReturn_ws "field" J_Field))
    .>>. pType
    .>>. (sepBy1 pVarName_ws (str_ws ","))
    .>> str_ws ";"
    |>> function (scope, jt),names -> names |> List.map (fun n -> (scope,jt,n)) 

let pClassBody = pComment >>. between poc pcc ((many pClassVariableDeclaration) .>>. (many1 pSubroutineDeclaration)) .>> ws |>> function v,s -> (List.collect id v, s)
let pClass = pComment >>. (str_ws "class") >>. pClassName .>>. pClassBody |>> function n,(v,s) -> { name = n; variables = v; subroutines = s; }


let pInput = ws >>. pClass .>> eof

let parseString str = run pInput str
let parseFile fileName = runParserOnFile pInput () fileName System.Text.Encoding.Default
