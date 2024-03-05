module public assembler.api

open FParsec.CharParsers
open assembler.parsers
open assembler.translator

let assemble input =
    match parseString input with
    | Success (results, _, _) ->
        let translated = translate results
        translated
    | Failure(msg, _, _) -> failwith msg