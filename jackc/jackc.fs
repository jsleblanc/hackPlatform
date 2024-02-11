module jackc.api

open System.IO
open FParsec.CharParsers
open jackc.codeGen
open jackc.validation

let parseFile (file:FileInfo) =
    match parser.parseFile file.FullName with
    | Success(jc, _, _) -> OK jc
    | Failure(msg, _, _) -> errorMsg file.Name msg

let compileFile file =
    compileClass >>= parseFile file

let compileFiles files =
    files |> List.map compileFile |> validation.Traverse