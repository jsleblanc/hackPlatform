module jackc.util

open System.IO
open System.Text
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
    
let writeCompiledCodeToDisk path (code:CompiledCode list) =
    Directory.CreateDirectory(path) |> ignore
    let f c =
        let fileName = Path.Combine(path, Path.ChangeExtension(c.name, ".vm"))
        File.WriteAllText(fileName, c.code)
        printfn $"Wrote {fileName}"
    code |> List.iter f
    
let printErrors (errors:Error list) =
    let f ctx e =
        printfn $"{ctx}:"
        e |> List.iter (fun i -> printfn $"\t{i.message}")
    errors |> List.groupBy (_.context) |> List.iter (fun (ctx,e) -> f ctx e)
    
let combineStrings (s:string list) =
    let sb = (StringBuilder(), s) ||> List.fold (_.AppendLine)
    sb.ToString()
    
let computeOutputPath inputPath outputPathOpt =
    let file = FileInfo(inputPath)
    let folder = DirectoryInfo(inputPath)
    match file.Exists, folder.Exists, outputPathOpt with
    | true, _, Some path -> Some (Path.Combine(path, Path.ChangeExtension(file.Name, ".vm")))
    | true, _, None -> Some (Path.ChangeExtension(file.FullName, ".vm"))
    | false, true, Some path -> Some path
    | false, true, None -> Some (Path.Combine(folder.FullName, "vm"))
    | _ -> None