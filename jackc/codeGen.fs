module jackc.codeGen

open jackc.state
open jackc.types
open jackc.symbolTable
open jackc.validation
open jackc.util

type SymbolTable = Map<string * VariableScope,SymbolEntry>

type CompilationState = {
    context: string
    labelCounter: int
    classSymbols: SymbolTableState
    subroutineSymbolTable: SymbolTable
    results: ValidationResult<string list> list
}

let emptyCompilationState = {
    context = ""
    labelCounter = 1
    classSymbols = initSymbolTableState
    subroutineSymbolTable = emptySymbolTable
    results = []
}

let setContext ctx = Stateful (fun state -> (), { state with context = ctx })
let getContext = Stateful (fun state -> state.context, state)
let incLabelCounter = Stateful (fun state -> state.labelCounter, { state with labelCounter = state.labelCounter + 1 })
let setClassSymbols c = Stateful (fun state -> (), { state with classSymbols = c })
let getClassSymbols = Stateful (fun state -> state.classSymbols, state)
let setSubroutineSymbolTable t = Stateful (fun state -> (), { state with subroutineSymbolTable = t })
let getSubroutineSymbolTable = Stateful (fun state -> state.subroutineSymbolTable, state)
let initStateWithSymbolTable t = { emptyCompilationState with subroutineSymbolTable = t }
let pushResult r = Stateful (fun state -> (), { state with results = state.results @ [r] })
let getResults = Stateful (fun state -> state.results, state)

let getNextLabel name =
    state {
        let! context = getContext
        let! lc = incLabelCounter
        return $"{context}.{name}${lc}"
    }

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

let rec compileExpression expr =
    state {
        let! context = getContext
        let! symbolTable = getSubroutineSymbolTable
        match expr with
        | J_ADD(exprLeft, exprRight) ->
            let! left = compileExpression exprLeft
            let! right = compileExpression exprRight
            return fold [left; right; OK ["add"]]
        | J_SUB(exprLeft, exprRight) ->
            let! left = compileExpression exprLeft
            let! right = compileExpression exprRight
            return fold [left; right; OK ["sub"]]
        | J_MUL(exprLeft, exprRight) ->
            let! left = compileExpression exprLeft
            let! right = compileExpression exprRight
            return fold [left; right; OK ["call Math.multiply 2"]]
        | J_DIV(exprLeft, exprRight) ->
            let! left = compileExpression exprLeft
            let! right = compileExpression exprRight
            return fold [left; right; OK ["call Math.divide 2"]]
        | J_AND(exprLeft, exprRight) ->
            let! left = compileExpression exprLeft
            let! right = compileExpression exprRight
            return fold [left; right; OK ["and"]]
        | J_OR(exprLeft, exprRight) ->
            let! left = compileExpression exprLeft
            let! right = compileExpression exprRight
            return fold [left; right; OK ["or"]]
        | J_LT(exprLeft, exprRight) ->
            let! left = compileExpression exprLeft
            let! right = compileExpression exprRight
            return fold [left; right; OK ["lt"]]
        | J_GT(exprLeft, exprRight) ->
            let! left = compileExpression exprLeft
            let! right = compileExpression exprRight
            return fold [left; right; OK ["gt"]]
        | J_EQ(exprLeft, exprRight) ->
            let! left = compileExpression exprLeft
            let! right = compileExpression exprRight
            return fold [left; right; OK ["eq"]]
        | J_NEG expr ->
            let! code = compileExpression expr
            return fold [code; OK ["neg"]]
        | J_NOT expr ->
            let! code = compileExpression expr
            return fold [code; OK ["not"]]
        | J_Constant_Int i -> return OK (compileConstantInt i)    
        | J_Constant_String str -> return errorMsg context "todo"
        | J_Constant_Boolean b -> return OK (compileConstantBoolean b)
        | J_Constant_Null -> return OK compileConstantNull
        | J_Constant_This -> return OK compileConstantThis
        | J_Variable name -> return compileVariable Push context name symbolTable
        | J_Array_Index(name, expr) -> return errorMsg context "todo"
        | J_Subroutine_Call(scope, name, expr) -> return errorMsg context "todo"        
    }

//This is to get around pushing intermediate results back into the state
//I couldn't figure out any other way to have a stateful function (one that
//needs access to my CompilationState type) process a list of items and return
//the results from processing each item as one big list.
//The only approaches I've had work so far are to push the results of each item
//as its processed into the state object before processing the next one, or
//this recursive non-sense below
let rec compileExpressions expressions =
    state {
        match expressions with
        | [] -> return []
        | e::tail ->
            let! code = compileExpression e
            let! next = compileExpressions tail
            return [code] @ next
    }

let rec compileStatements statements =
    state {
        match statements with
        | [] -> return []
        | s::tail ->
            let! code = compileStatement s
            let! next = compileStatements tail
            return [code] @ next
    }

and compileStatement statement =
    state {
        let! context = getContext
        let! symbolTable = getSubroutineSymbolTable
        match statement with
        | J_Let expr ->
            match expr with
            | J_EQ (J_Variable name, exprRight) ->
                let variableAssignment = compileVariable Pop context name symbolTable
                let! valueToAssign = compileExpression exprRight
                return fold [valueToAssign; variableAssignment]
            | J_EQ (J_Array_Index (name, indexExpr), exprRight) -> return errorMsg context "todo"
            | _ -> return errorMsg context $"Unsupported expression in \"let\" statement: {expr}"
        | J_If_Else(condExpr, conditionStatements, elseStatements) ->
            let! conditionExpressionCode = compileExpression condExpr
            let! conditionStatementsCode = compileStatements conditionStatements
            let! label1 = getNextLabel "IF_ELSE"
            match elseStatements with
            | [] ->
                return fold [
                    conditionExpressionCode
                    OK ["not";$"if-goto {label1}"]
                    fold conditionStatementsCode
                    OK [$"label {label1}"]]
            | xs ->
                let! label2 = getNextLabel "IF_ELSE"
                let! elseStatementsCode = compileStatements xs            
                return fold [
                    conditionExpressionCode
                    OK [ "not"; $"if-goto {label1}"]
                    fold conditionStatementsCode
                    OK [$"goto {label2}"; $"label {label1}"]
                    fold elseStatementsCode
                    OK [$"label {label2}"]]                
        | J_While(condExpr, statements) -> return errorMsg context "todo"
        | J_Do(scope, name, parameterExpressions) ->            
            let functionName =
                match scope with
                | Some s -> $"{s}.{name}"
                | None -> name
            let! code = compileExpressions parameterExpressions
            return fold [fold code; OK [$"call {functionName} {parameterExpressions.Length}"; "pop temp 0"]]            
        | J_Return exprOption ->
            match exprOption with
            | Some expr ->
                let! code = compileExpression expr
                return fold [code; OK ["return"]]
            | None -> return OK ["push constant 0"; "return"]        
    }

let compileSubroutine (s:JackSubroutine) =
    state {
        let! context = getContext
        let! classSymbols = getClassSymbols
        do! setContext $"{context}.{s.name}"
        do! setSubroutineSymbolTable (buildSymbolsForSubroutine classSymbols s)
        
        for statement in s.body do
            let! code = compileStatement statement
            do! pushResult code
        
        let! results = getResults
        return (fold results)
    }

let compileClassStateful (c:JackClass) =
    state {
        do! setClassSymbols (buildSymbolsForClass c)        
        for subroutine in c.subroutines do
            do! setContext c.name
            let! code = compileSubroutine subroutine
            do! pushResult code        
        let! results = getResults
        return (fold results)
    }

let compileClass (c:JackClass) =
    let result,state = run emptyCompilationState (compileClassStateful c)
    match result with
    //| OK code -> OK { name = c.name; code = combineStrings code }
    | OK _ -> errorMsg c.name "todo" 
    | Invalid e -> Invalid e