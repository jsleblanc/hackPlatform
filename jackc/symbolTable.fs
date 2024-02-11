module jackc.symbolTable

open jackc.types

type VariableScope =
    | ClassScope
    | SubroutineScope

type SegmentIndex = int

type Segment =
    | This of SegmentIndex
    | Static of SegmentIndex
    | Local of SegmentIndex
    | Argument of SegmentIndex
    with
        member this.Inc() =
            match this with
            | This i -> This (i + 1)
            | Static i -> Static (i + 1)
            | Local i -> Local (i + 1)
            | Argument i -> Argument (i + 1)
            
type SymbolEntry = {
    name: string
    scope: VariableScope
    varType: JackTypes
    segment: Segment
}

type SymbolTableKey = string * VariableScope
type SymbolTable = Map<SymbolTableKey, SymbolEntry>

type SymbolTableState = {
    entries: SymbolEntry list
    segThis: Segment
    segStatic: Segment
    segArgument: Segment
    segLocal: Segment    
}

let initSymbolTableState = {
    entries = []
    segThis = This 0
    segStatic = Static 0
    segLocal = Local 0
    segArgument = Argument 0 
}

let genClassSymbolTable c =
    let folder state var =
        let scope, jType, name = var
        let varSeg =
            match scope with
            | J_Field -> state.segThis
            | J_Static -> state.segStatic
        let entry = {
            name = name
            scope = ClassScope 
            varType = jType
            segment = varSeg 
        }
        let newState =
            match scope with
            | J_Field -> { state with segThis = state.segThis.Inc() }
            | J_Static -> { state with segStatic = state.segStatic.Inc() }
        { newState with entries = newState.entries @ [entry] }    
    c.variables |> List.fold folder initSymbolTableState

let genSubroutineSymbolTable initialState s = 
    let folder state var =
        let scope, jType, name = var
        let varSeg =
            match scope with
            | J_Argument -> state.segArgument
            | J_Local -> state.segLocal
        let entry = {
            name = name
            scope = SubroutineScope
            varType = jType
            segment = varSeg 
        }
        let newState =
            match scope with
            | J_Argument -> { state with segArgument = state.segArgument.Inc() }
            | J_Local -> { state with segLocal = state.segLocal.Inc() }
        { newState with entries = newState.entries @ [entry] }
    let state = s.parameters |> List.fold folder initialState
    s.variables |> List.fold folder state
    
let symbolStateToTable state =
    state.entries |> List.map (fun e -> ((e.name, e.scope), e)) |> Map.ofList

let buildSymbolsForClass c = genClassSymbolTable c

let buildSymbolsForSubroutine classSymbols subroutine =
    genSubroutineSymbolTable classSymbols subroutine |> symbolStateToTable
    
let symbolLookup table name =
    let local = (name, SubroutineScope)
    let class_ = (name, ClassScope)
    match Map.tryFind local table, Map.tryFind class_ table with
    | Some e, _ -> Some e
    | None, Some e -> Some e
    | None, None -> None
    
let emptySymbolTable = symbolStateToTable initSymbolTableState