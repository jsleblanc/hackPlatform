module jackc.validation

type Error = {
    context: string
    message: string
}

type ValidationResult<'a> =
    | OK of 'a
    | Invalid of Error list
    
let error e = Invalid [e]

let errorMsg ctx msg =
    error { context = ctx; message = msg }

let errors v =
    match v with
    | OK _ -> []
    | Invalid errors -> errors

let choose v =
    match v with
    | OK a -> Some a
    | _ -> None

let map f v =
    match v with
    | OK a -> f a
    | Invalid _ -> v

let mapError f v =
    match v with
    | OK _ -> v
    | Invalid e -> Invalid (f e)

let bind f v =
    match v with
    | OK a -> f a
    | Invalid e -> Invalid e
    
let apply fVal xVal =
    match fVal, xVal with
    | OK f, OK v -> OK (f v)
    | OK _, Invalid ve -> Invalid ve
    | Invalid fe, OK _ -> Invalid fe
    | Invalid fe, Invalid ve -> Invalid (fe @ ve)
            
let fold vs =    
    let allErrors = vs |> Seq.collect errors |> Seq.toList
    if Seq.length allErrors > 0 then
        Invalid allErrors
    else
        let allValues = vs |> Seq.choose choose |> Seq.toList
        OK allValues

let emptyError = {
    context = ""
    message = "" 
}

let (<*>) = apply
let (>>=) = bind
    
type ValidationBuilder() =
    member this.Bind(x, f) = bind f x
    member this.BindReturn(v, f) = bind f v
    member this.Return(x) = OK x
    member this.Fail(e) = error e
    member this.ReturnFrom(x) = OK x
    member this.Zero() = Invalid [emptyError]
    //member this.Combine(a,b) = this.Bind(a, fun()-> b)
    member this.Traverse(vs) = fold vs
    member this.MergeSources(x, y) =
        match x,y with
        | OK ax, OK ay -> OK (ax,ay)
        | Invalid ex, OK _ -> Invalid ex
        | OK _, Invalid ey -> Invalid ey
        | Invalid ex, Invalid ey -> Invalid (ex @ ey)
    
                
let validation = ValidationBuilder()