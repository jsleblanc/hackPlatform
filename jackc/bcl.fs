module jackc.bcl

open System.IO
open System.Reflection

let readStream (s:Stream) =
    let sr = new StreamReader(s)
    sr.ReadToEnd()

let assembly = Assembly.GetExecutingAssembly()
let nspace = assembly.GetExportedTypes().[0].Namespace
let sysJack = readStream (assembly.GetManifestResourceStream("jackc.BCL.Sys.jack"))
let memoryJack = readStream (assembly.GetManifestResourceStream("jackc.BCL.Memory.jack"))
