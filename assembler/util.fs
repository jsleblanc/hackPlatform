module assembler.util

open System.IO

let dumpSymbolsToDisk (map:Map<string, uint16>) fileName =
    let lines = map |> Map.toList |> List.map (fun (k,v) -> $"{k},{v},") 
    File.WriteAllLines(fileName, lines)