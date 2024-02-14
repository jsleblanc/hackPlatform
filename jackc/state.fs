module jackc.state

/// A stateful computation.
type Stateful<'state, 'result> = Stateful of ('state -> 'result * 'state)

let inline run state x = let (Stateful(f)) = x in f state
let get = Stateful(fun s -> s, s)
let put newState = Stateful(fun _ -> (), newState)
let map f s = Stateful(fun (state: 's) ->
    let x, state = run state s
    f x, state)

/// 'result -> Stateful<'state, 'result>
let ret result =
    Stateful (fun state -> (result, state))

/// ('a -> Stateful<'state, 'b>) -> Stateful<'state, 'a> -> Stateful<'state, 'b>
let bind binder stateful =
    Stateful (fun state ->
        let result, state' = stateful |> run state
        binder result |> run state')

type StatefulBuilder() =
    let (>>=) stateful binder = bind binder stateful
    member _.Return(result) = ret result
    member _.ReturnFrom(stateful) = stateful
    member _.Bind(stateful, binder) = stateful >>= binder
    member _.Zero() = ret ()
    member _.Combine(statefulA, statefulB) = statefulA >>= (fun _ -> statefulB)
    member _.Delay(f) = f ()
    member this.For (seq, f: 'a -> Stateful<'s, 'b>) =
        seq
        |> List.map f
        |> List.reduceBack (fun x1 x2 -> this.Combine (x1, x2))
    member this.While (f, x) =
        if f () then this.Combine (x, this.While (f, x))
        else this.Zero ()    

let state = StatefulBuilder()