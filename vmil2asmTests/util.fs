module vmil2asmTests.util

open System.Text

let fold (asm:string list) = (StringBuilder(), asm) ||> List.fold (_.AppendLine) |> (_.ToString())

type ClassDataBase(generator : obj [] seq) = 
    interface seq<obj []> with
        member this.GetEnumerator() = generator.GetEnumerator()
        member this.GetEnumerator() = 
            generator.GetEnumerator() :> System.Collections.IEnumerator