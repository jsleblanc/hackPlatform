module jackcTests.util

open System.IO

type ClassDataBase(generator : obj [] seq) = 
    interface seq<obj []> with
        member this.GetEnumerator() = generator.GetEnumerator()
        member this.GetEnumerator() = 
            generator.GetEnumerator() :> System.Collections.IEnumerator

let listFilesToObjSequence path pattern =
    let files =
        Directory.GetFiles(path, pattern, SearchOption.AllDirectories)
        |> Seq.map (fun s -> [| s :> obj |])
    files
            
type FilePathBase(path : string, pattern : string) =
    interface seq<obj []> with
        member this.GetEnumerator() = (listFilesToObjSequence path pattern).GetEnumerator()
        member this.GetEnumerator() =
            (listFilesToObjSequence path pattern).GetEnumerator() :> System.Collections.IEnumerator
            
            