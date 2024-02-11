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

let rec compileExpression expr =
    match expr with
    | J_ADD(exprLeft, exprRight) ->
        let left = compileExpression exprLeft
        let right = compileExpression exprRight
        left @ right @ ["add"]
    | J_SUB(exprLeft, exprRight) ->
        let left = compileExpression exprLeft
        let right = compileExpression exprRight
        left @ right @ ["sub"]        
    | J_MUL(exprLeft, exprRight) ->
        let left = compileExpression exprLeft
        let right = compileExpression exprRight
        left @ right @ ["call Math.multiply 2"]
    | J_DIV(exprLeft, exprRight) ->
        let left = compileExpression exprLeft
        let right = compileExpression exprRight
        left @ right @ ["call Math.divide 2"]
    | J_AND(exprLeft, exprRight) ->
        let left = compileExpression exprLeft
        let right = compileExpression exprRight
        left @ right @ ["and"]
    | J_OR(exprLeft, exprRight) ->
        let left = compileExpression exprLeft
        let right = compileExpression exprRight
        left @ right @ ["or"]
    | J_LT(exprLeft, exprRight) ->
        let left = compileExpression exprLeft
        let right = compileExpression exprRight
        left @ right @ ["lt"]        
    | J_GT(exprLeft, exprRight) ->
        let left = compileExpression exprLeft
        let right = compileExpression exprRight
        left @ right @ ["gt"]        
    | J_EQ(exprLeft, exprRight) ->
        let left = compileExpression exprLeft
        let right = compileExpression exprRight
        left @ right @ ["eq"]
    | J_NEG expr ->
        let code = compileExpression expr
        code @ ["neg"]
    | J_NOT expr ->
        let code = compileExpression expr
        code @ ["not"]
    | J_Constant_Int i -> compileConstantInt i    
    | J_Constant_String s -> failwith "todo"
    | J_Constant_Boolean b -> compileConstantBoolean b
    | J_Constant_Null -> compileConstantNull
    | J_Constant_This -> compileConstantThis
    | J_Variable s -> failwith "todo"
    | J_Array_Index(name, expr) -> failwith "todo"
    | J_Subroutine_Call(scope, name, expr) -> failwith "todo"
    

let compileClass (c:JackClass) =
    let classSymbols = buildSymbolsForClass c
    
    errorMsg c.name "not implemented yet"