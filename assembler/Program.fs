
open assembler.parsers

let a = parseAssemblyFile @"/Users/josephleblanc/Documents/Code/nand2tetris/projects/06/max/Max.asm" System.Text.Encoding.UTF8
let b = parseAssemblyFile @"/Users/josephleblanc/Documents/Code/nand2tetris/projects/06/pong/Pong.asm" System.Text.Encoding.UTF8
// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"