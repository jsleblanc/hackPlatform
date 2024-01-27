module public assembler.api

open FParsec.CharParsers
open assembler.parsers
open assembler.translator

let filterInstructions pr =
    match pr with
    | Code i -> Some i
    | Comment _ -> None

let assemble input =
    match parseString input with
    | Success (results, _, _) ->
        let code = results |> List.map filterInstructions |> List.choose id
        let translated = translate code
        translated
    | Failure(msg, _, _) -> failwith msg