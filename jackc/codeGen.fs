module jackc.codeGen

open jackc.types
open jackc.symbolTable
open jackc.validation

let compileConstantInt i = [$"push constant {i}"]

let compileConstantBoolean b =
    match b with
    | true -> ["push constant 1"; "neg"]
    | false -> ["push constant 0"]

let compileConstantNull = ["push constant 0"]

let compileConstantThis = ["push pointer 0"]

let rec compileExpression context expr symbolTable =
    let fold = List.fold validation.MergeLists (OK [])
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
    | J_Constant_String str -> failwith "todo"
    | J_Constant_Boolean b -> OK (compileConstantBoolean b)
    | J_Constant_Null -> OK compileConstantNull
    | J_Constant_This -> OK compileConstantThis
    | J_Variable name ->
        match symbolLookup symbolTable name with
        | Some symbol ->
            match symbol.segment with
            | Argument i -> OK [$"push argument {i}"]
            | This i -> OK [$"push this {i}"]
            | Static i -> OK [$"push static {i}"]
            | Local i -> OK [$"push local {i}"]
        | None -> errorMsg context $"Could not resolve symbol \"{name}\""
    | J_Array_Index(name, expr) -> failwith "todo"
    | J_Subroutine_Call(scope, name, expr) -> failwith "todo"
    

let compileClass (c:JackClass) =
    let classSymbols = buildSymbolsForClass c
    
    errorMsg c.name "not implemented yet"