module jackc.api

open System.IO
open FParsec.CharParsers
open jackc.codeGen
open jackc.validation

let parseFile (file:FileInfo) =
    match parser.parseFile file.FullName with
    | Success(jc, _, _) -> OK jc
    | Failure(msg, _, _) -> errorMsg file.Name msg

let parseString str =
    match parser.parseString str with
    | Success(jc, _, _) -> OK jc
    | Failure(msg, _, _) -> errorMsg "" msg

let compileFile (file: FileInfo) =
    let contextualizeError (e:Error) = { e with context = $"{file.FullName} -- {e.context}"; }
    compileClass >>= parseFile file |> mapError (List.map contextualizeError)

let compileString str =
    compileClass >>= parseString str

let compileFiles files =
    files |> List.map compileFile |> validation.Traverse