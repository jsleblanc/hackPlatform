module jackc.api

open System.IO
open FParsec.CharParsers
open jackc.codeGen
open jackc.validation
open jackc.bcl

let private parseFile (file:FileInfo) =
    match parser.parseFile file.FullName with
    | Success(jc, _, _) -> OK jc
    | Failure(msg, _, _) -> errorMsg file.Name msg

let private parseString str =
    match parser.parseString str with
    | Success(jc, _, _) -> OK jc
    | Failure(msg, _, _) -> errorMsg "" msg

let private compileFile (file: FileInfo) =
    let contextualizeError (e:Error) = { e with context = $"{file.FullName} -- {e.context}"; }
    compileClass >>= parseFile file |> mapError (List.map contextualizeError)

let compileString str =
    compileClass >>= parseString str

let private compileBcl =
    [sysJack; memoryJack] |> List.map compileString

let compileFiles files =
    let bcl = compileBcl
    files |> List.map compileFile |> List.insertManyAt 0 bcl |> validation.Traverse