module vmil2asm.util

open System.IO
open vmil2asm.types

let findInputFiles inputPath =
    let file = FileInfo(inputPath)
    let folder = DirectoryInfo(inputPath)
    if file.Exists = true then
       let outputName = Path.ChangeExtension(file.FullName, ".asm")
       Some { outputName = outputName; inputFiles = [file] }
    else if folder.Exists then
        let files = folder.EnumerateFiles("*.vm", SearchOption.AllDirectories) |> List.ofSeq
        let outputName = Path.Combine(folder.FullName, Path.ChangeExtension(folder.Name, ".asm"))
        match files with
        | [] -> None
        | xs -> Some { outputName = outputName; inputFiles = xs }
    else None
    
let overrideOutputName name overriddenName =
    match overriddenName with
    | Some s -> s
    | None -> name