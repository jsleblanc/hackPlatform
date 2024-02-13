module jackc.codeGen

open jackc.types
open jackc.symbolTable
open jackc.validation
open jackc.util

type StackDirection =
    | Push
    | Pop

let stackDirectionCommand dir =
    match dir with
    | Push -> "push"
    | Pop -> "pop"

let compileConstantInt i = [$"push constant {i}"]

let compileConstantBoolean b =
    match b with
    | true -> ["push constant 1"; "neg"]
    | false -> ["push constant 0"]

let compileConstantNull = ["push constant 0"]

let compileConstantThis = ["push pointer 0"]

let compileVariable dir context name symbolTable =
    match symbolLookup symbolTable name with
    | Some symbol ->
        match symbol.segment with
        | Argument i -> OK [$"{stackDirectionCommand dir} argument {i}"]
        | This i -> OK [$"{stackDirectionCommand dir} this {i}"]
        | Static i -> OK [$"{stackDirectionCommand dir} static {i}"]
        | Local i -> OK [$"{stackDirectionCommand dir} local {i}"]
    | None -> errorMsg context $"Could not resolve symbol \"{name}\""    

let fold = List.fold validation.MergeLists (OK [])

let rec compileExpression context expr symbolTable =
    match expr with
    | J_ADD(exprLeft, exprRight) ->
        let left = compileExpression context exprLeft symbolTable
        let right = compileExpression context exprRight symbolTable
        fold [left; right; OK ["add"]]
    | J_SUB(exprLeft, exprRight) ->
        let left = compileExpression context exprLeft symbolTable
        let right = compileExpression context exprRight symbolTable
        fold [left; right; OK ["sub"]]
    | J_MUL(exprLeft, exprRight) ->
        let left = compileExpression context exprLeft symbolTable
        let right = compileExpression context exprRight symbolTable
        fold [left; right; OK ["call Math.multiply 2"]]
    | J_DIV(exprLeft, exprRight) ->
        let left = compileExpression context exprLeft symbolTable
        let right = compileExpression context exprRight symbolTable
        fold [left; right; OK ["call Math.divide 2"]]
    | J_AND(exprLeft, exprRight) ->
        let left = compileExpression context exprLeft symbolTable
        let right = compileExpression context exprRight symbolTable
        fold [left; right; OK ["and"]]
    | J_OR(exprLeft, exprRight) ->
        let left = compileExpression context exprLeft symbolTable
        let right = compileExpression context exprRight symbolTable
        fold [left; right; OK ["or"]]
    | J_LT(exprLeft, exprRight) ->
        let left = compileExpression context exprLeft symbolTable
        let right = compileExpression context exprRight symbolTable
        fold [left; right; OK ["lt"]]
    | J_GT(exprLeft, exprRight) ->
        let left = compileExpression context exprLeft symbolTable
        let right = compileExpression context exprRight symbolTable
        fold [left; right; OK ["gt"]]
    | J_EQ(exprLeft, exprRight) ->
        let left = compileExpression context exprLeft symbolTable
        let right = compileExpression context exprRight symbolTable
        fold [left; right; OK ["eq"]]
    | J_NEG expr ->
        let code = compileExpression context expr symbolTable
        fold [code; OK ["neg"]]
    | J_NOT expr ->
        let code = compileExpression context expr symbolTable
        fold [code; OK ["not"]]
    | J_Constant_Int i -> OK (compileConstantInt i)    
    | J_Constant_String str -> errorMsg context "todo"
    | J_Constant_Boolean b -> OK (compileConstantBoolean b)
    | J_Constant_Null -> OK compileConstantNull
    | J_Constant_This -> OK compileConstantThis
    | J_Variable name -> compileVariable Push context name symbolTable
    | J_Array_Index(name, expr) -> errorMsg context "todo"
    | J_Subroutine_Call(scope, name, expr) -> errorMsg context "todo"
    
let rec compileStatement context symbolTable statement =
    match statement with
    | J_Let expr ->
        match expr with
        | J_EQ (J_Variable name, exprRight) ->
            let variableAssignment = compileVariable Pop context name symbolTable
            let valueToAssign = compileExpression context exprRight symbolTable
            fold [valueToAssign; variableAssignment]
        | J_EQ (J_Array_Index (name, indexExpr), exprRight) -> errorMsg context "todo"
        | _ -> errorMsg context $"Unsupported expression in \"let\" statement: {expr}"
    | J_If_Else(condExpr, jackStatements, statements) -> errorMsg context "todo"
    | J_While(condExpr, jackStatements) -> errorMsg context "todo"
    | J_Do(scope, name, jackExpressions) -> errorMsg context "todo"
    | J_Return exprOption ->
        match exprOption with
        | Some expr ->
            let code = compileExpression context expr symbolTable
            fold [code; OK ["return"]]
        | None -> OK ["push constant 0"; "return"]

let compileSubroutine className symbols (s:JackSubroutine) =
    let context = $"{className}.{s.name} ({s.subType})"
    let symbolTable = buildSymbolsForSubroutine symbols s
    s.body |> List.map (compileStatement context symbolTable) |> fold

let compileClass (c:JackClass) =
    let classSymbols = buildSymbolsForClass c
    let result = c.subroutines |> List.map (compileSubroutine c.name classSymbols) |> fold
    match result with
    //| OK code -> OK { name = c.name; code = combineStrings code }
    | OK _ -> errorMsg c.name "todo" 
    | Invalid e -> Invalid e