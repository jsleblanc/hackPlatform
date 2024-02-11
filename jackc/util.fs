module jackc.util

open System.IO
open jackc.validation
open jackc.types

let findInputFiles inputPath =
    let file = FileInfo(inputPath)
    let folder = DirectoryInfo(inputPath)
    if file.Exists = true then
       [file] 
    else if folder.Exists then
        folder.EnumerateFiles("*.jack", SearchOption.AllDirectories) |> List.ofSeq
    else []
    
let writeCompiledCodeToDisk (code:CompiledCode list) =
    let f c =
        File.WriteAllText(c.name, c.code)
        printfn $"Wrote {c.name}"
    code |> List.iter f
    
let printErrors (errors:Error list) =
    let f ctx e =
        printfn $"{ctx}:"
        e |> List.iter (fun i -> printfn $"\t{i.message}")
    errors |> List.groupBy (_.context) |> List.iter (fun (ctx,e) -> f ctx e)