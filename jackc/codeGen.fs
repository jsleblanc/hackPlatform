module jackc.codeGen

open jackc.state
open jackc.types
open jackc.symbolTable
open jackc.validation
open jackc.util

[<Literal>]
let LABEL_IF_ELSE = "IF_ELSE"

[<Literal>]
let LABEL_WHILE = "WHILE"

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

let mapSymbolToSegment name =
    state {
        let! symbolTable = getSubroutineSymbolTable
        let segmentString = 
            match symbolLookup symbolTable name with
            | Some symbol ->
                match symbol.segment with
                | Argument i -> Some $"argument {i}"
                | This i -> Some $"this {i}"
                | Static i -> Some $"static {i}"
                | Local i -> Some $"local {i}"
            | None -> None
        return segmentString
    }

let compileVariable dir name =
    state {
        let! context = getContext
        let! segment = mapSymbolToSegment name
        match segment with
        | Some segmentString -> return OK [$"{stackDirectionCommand dir} {segmentString}"]
        | None -> return errorMsg context $"Could not resolve symbol \"{name}\""
    }

let fold = List.fold validation.MergeLists (OK [])

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

and compileExpression expr =
    state {
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
        | J_Constant_String str -> return OK ["//Expression: string constant TODO"]
        | J_Constant_Boolean b -> return OK (compileConstantBoolean b)
        | J_Constant_Null -> return OK compileConstantNull
        | J_Constant_This -> return OK compileConstantThis
        | J_Variable name -> return! compileVariable Push name
        | J_Array_Index(name, expr) -> return OK ["//Expression: array index TODO"]
        | J_Subroutine_Call(scope, name, parameters) ->
            return OK ["//Expression: subroutine call TODO"]
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
                let! variableAssignment = compileVariable Pop name
                let! valueToAssign = compileExpression exprRight
                return fold [valueToAssign; variableAssignment]
            | J_EQ (J_Array_Index (name, indexExpr), exprRight) -> return OK ["//Statement: Let statement assigning value to array position TODO"]
            | _ -> return errorMsg context $"Unsupported expression in \"let\" statement: {expr}"
        | J_If_Else(condExpr, conditionStatements, elseStatements) ->
            let! conditionExpressionCode = compileExpression condExpr
            let! label1 = getNextLabel LABEL_IF_ELSE
            let! conditionStatementsCode = compileStatements conditionStatements
            match elseStatements with
            | [] ->
                return fold [
                    conditionExpressionCode
                    OK ["not";$"if-goto {label1}"]
                    fold conditionStatementsCode
                    OK [$"label {label1}"]]
            | xs ->
                let! label2 = getNextLabel LABEL_IF_ELSE
                let! elseStatementsCode = compileStatements xs            
                return fold [
                    conditionExpressionCode
                    OK [ "not"; $"if-goto {label1}"]
                    fold conditionStatementsCode
                    OK [$"goto {label2}"; $"label {label1}"]
                    fold elseStatementsCode
                    OK [$"label {label2}"]]                
        | J_While(condExpr, statements) ->
            let! label1 = getNextLabel LABEL_WHILE
            let! label2 = getNextLabel LABEL_WHILE
            let! conditionExpressionCode = compileExpression condExpr
            let! statementsCode = compileStatements statements
            return fold [
                OK [$"label {label1}"]
                conditionExpressionCode
                OK ["not"; $"if-goto {label2}"]
                fold statementsCode
                OK [$"goto {label1}";$"label {label2}"]
            ]
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
        let! statementsCode = compileStatements s.body
        
        let initCode =
            match s.subType with
            | J_Method -> OK ["push argument 0";"pop pointer 0"] //setup THIS pointer
            | J_Function -> OK [] //functions are static
            | J_Constructor -> errorMsg context "Internal error: Constructors must be compiled separately from methods and functions"
        
        return fold [
            OK [$"function {context}.{s.name} {s.variables.Length}"]
            initCode
            fold statementsCode
        ]
    }

let compileConstructor (c:JackClass) (s:JackSubroutine) =
    state {
        let! context = getContext
        let! classSymbols = getClassSymbols
        do! setContext $"{context}.{s.name}"
        do! setSubroutineSymbolTable (buildSymbolsForSubroutine classSymbols s)
        
        let checkReturnType = 
            match s.returnType with
            | J_ReturnType (J_Class className) ->
                if className = c.name then OK [] else errorMsg c.name "Constructor return type must be that of the class it belongs to"
            | _ -> OK []
        
        let checkReturnsThis =
            match s.body |> List.tryLast with
            | Some (J_Return (Some J_Constant_This)) -> OK []
            | _ -> errorMsg c.name "Constructor must return THIS"
        
        let variableFilter v =
            match v with
            | J_Static, _, _ -> true
            | J_Field, _, _ -> false
        
        let staticVars,fieldVars = c.variables |> List.partition variableFilter
        let! statementsCode = compileStatements s.body
                
        return fold [
            checkReturnType
            checkReturnsThis
            OK [
                $"function {c.name}.{s.name} {s.variables.Length}"
                $"push constant {fieldVars.Length}"
                "call Memory.alloc 1"
                "pop pointer 0"
            ]
            fold statementsCode
        ]
    }

let compileClassStateful (c:JackClass) =
    state {
        do! setClassSymbols (buildSymbolsForClass c)        
        let constructors, subroutines = c.subroutines |> List.partition (fun s -> s.subType = J_Constructor)
       
        if not (List.isEmpty constructors) then
            for constructor in constructors do
                do! setContext c.name
                let! code = compileConstructor c constructor
                do! pushResult code
        
        if not (List.isEmpty subroutines) then
            for subroutine in subroutines do
                do! setContext c.name
                let! code = compileSubroutine subroutine
                do! pushResult code
                
        let! results = getResults
        return (fold results)
    }

let compileClass (c:JackClass) =
    let result,_ = run emptyCompilationState (compileClassStateful c)
    match result with
    | OK code -> OK { name = c.name; code = combineStrings code }
    | Invalid e -> Invalid e