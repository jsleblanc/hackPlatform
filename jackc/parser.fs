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
let pSubroutineScope = pIdentifier .>> str "." |>> function s -> JackSubroutineCallScope s
let pVarName = pIdentifier |>> function name -> JackVariableName name
let pVarName_ws = pIdentifier_ws |>> function name -> JackVariableName name

let pop = skipChar_ws '(' //parser skip open parenthesis
let pcp = skipChar_ws ')' //parser skip close parenthesis
let pob = skipChar_ws '['
let pcb = skipChar_ws ']'

//Terms and Expressions
let opp = OperatorPrecedenceParser<JackExpression,unit,unit>()
let expr = opp.ExpressionParser

let pTermVariable = pVarName_ws |>> function v -> J_Variable v
let pTermConstantInt = pint16 .>> ws |>> function i -> J_Constant_Int i
let pTermConstantString = between (str "\"") (str "\"") (manySatisfy (fun c -> c <> '"')) .>> ws |>> function s -> J_Constant_String s
let pTermConstantBoolean = choiceL [ (stringReturn_ws "true" (J_Constant_Boolean true)); (stringReturn_ws "false" (J_Constant_Boolean false)) ] "boolean" .>> ws
let pTermConstantNull = stringReturn_ws "null" J_Constant_Null .>> ws
let pTermConstantThis = stringReturn_ws "this" J_Constant_This .>> ws
let pTermArrayIndexer = pVarName .>>. (between pob pcb expr) .>> ws |>> function n,e -> J_Array_Index (n, e)
let pTermsList = between pop pcp (sepBy expr (str_ws ",")) .>> ws
let pTermSubroutineLocalCall = pSubroutineName .>>. pTermsList |>> function n,p -> J_Subroutine_Call (None, n, p)
let pTermSubroutineScopedCall = pSubroutineScope .>>. pSubroutineName .>>. pTermsList |>> function (s,n),p -> J_Subroutine_Call (Some s, n, p)

let pTerms =
    choiceL [
        pTermConstantInt
        pTermConstantString
        pTermConstantBoolean
        pTermConstantNull
        pTermConstantThis
        attempt pTermSubroutineScopedCall
        attempt pTermSubroutineLocalCall
        attempt pTermArrayIndexer
        pTermVariable
    ] "terms" .>> ws

let term = between pop pcp expr <|> pTerms
opp.TermParser <- term

type Assoc = Associativity
opp.AddOperator(PrefixOperator("-", ws, 5, true, J_NEG))
opp.AddOperator(PrefixOperator("~", ws, 5, true, J_NOT))
opp.AddOperator(InfixOperator("&", ws, 4, Assoc.Left, fun x y -> J_AND (x,y)))
opp.AddOperator(InfixOperator("|", ws, 4, Assoc.Left, fun x y -> J_OR (x,y)))
opp.AddOperator(InfixOperator("/", ws, 3, Assoc.Left, fun x y -> J_DIV (x,y)))
opp.AddOperator(InfixOperator("*", ws, 3, Assoc.Left, fun x y -> J_MUL (x,y)))
opp.AddOperator(InfixOperator("+", ws, 2, Assoc.Left, fun x y -> J_ADD (x,y)))
opp.AddOperator(InfixOperator("-", ws, 2, Assoc.Left, fun x y -> J_SUB (x,y)))
opp.AddOperator(InfixOperator("=", ws, 1, Assoc.None, fun x y -> J_EQ (x,y)))
opp.AddOperator(InfixOperator("<", ws, 1, Assoc.Left, fun x y -> J_LT (x,y)))
opp.AddOperator(InfixOperator(">", ws, 1, Assoc.Left, fun x y -> J_GT (x,y)))

let pExpression = expr

//Statements
let pStatementImpl, pStatementRef = createParserForwardedToRef()

let poc = skipChar_ws '{'
let pcc = skipChar_ws '}'

let pStatementReturn = str_ws "return" >>. (opt pExpression) .>> str_ws ";" |>> function e -> J_Return e
let pStatementLet = (str_ws "let" >>. pExpression) .>> str_ws ";" |>> function e -> J_Let e
let pStatementBlock = between poc pcc (pComment >>. many pStatementImpl) .>> ws
let pStatementWhile = (str_ws "while" >>. pExpression) .>>. pStatementBlock |>> function e,s -> J_While (e,s)
let pStatementIfElse = (str_ws "if") >>. pExpression .>>. pStatementBlock .>>. ((str_ws "else") >>. pStatementBlock) |>> function (c,sl),esl -> J_If_Else (c,sl, esl)
let pStatementIf = (str_ws "if") >>. pExpression .>>. pStatementBlock |>> function c,sl -> J_If_Else (c,sl,[])
let pStatementDoLocalSubroutineCall = (str_ws "do") >>. pSubroutineName .>>. pTermsList .>> (str_ws ";") |>> function n,es -> J_Do (None, n, es)
let pStatementDoScopedSubroutineCall = (str_ws "do") >>. pSubroutineScope .>>. pSubroutineName .>>. pTermsList .>> (str_ws ";") |>> function (s,n),es -> J_Do (Some s, n, es)

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
let pParameterList = between (str_ws "(") (str_ws ")") (sepBy (pType .>>. pVarName_ws |>> function jt,name -> (J_Argument,jt,name)) (str_ws ","))

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
    pComment
    >>. (str_ws "var")
    >>. pType
    .>>. (sepBy1 pVarName_ws (str_ws ","))
    .>> str_ws ";"
    .>> pComment
    |>> function jt,names -> names |> List.map (fun n -> (J_Local,jt,n))

let pSubroutineBody = pComment >>. between poc pcc ((pComment >>. many pSubroutineVariableDeclaration) .>>. (pComment >>. many pStatementImpl)) .>> ws |>> function v,s -> (List.collect id v, s)

let pSubroutineDeclaration =
    pComment >>. pipe5 pSubroutineType pSubroutineReturnType pSubroutineName pParameterList pSubroutineBody (fun a b c d (vars,statements) -> {subType = a; returnType = b; name = c; parameters = d; variables = vars; body = statements; }) .>> pComment
            
//Classes
let pClassVariableDeclaration =
    pComment
    >>. ((stringReturn_ws "static" J_Static) <|> (stringReturn_ws "field" J_Field))
    .>>. pType
    .>>. (sepBy1 pVarName_ws (str_ws ","))
    .>> str_ws ";"
    .>> pComment
    |>> function (scope, jt),names -> names |> List.map (fun n -> (scope,jt,n)) 

let pClassBody = pComment >>. between poc pcc ((pComment >>. many pClassVariableDeclaration) .>>. (many1 pSubroutineDeclaration)) .>> ws |>> function v,s -> (List.collect id v, s)
let pClass = pComment >>. (str_ws "class") >>. pClassName .>>. pClassBody .>> ws |>> function n,(v,s) -> { name = n; variables = v; subroutines = s; }


let pInput = ws >>. pClass .>> eof

let parseString str = run pInput str
let parseFile fileName = runParserOnFile pInput () fileName System.Text.Encoding.Default
