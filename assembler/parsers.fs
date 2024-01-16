module assembler.parsers

open FParsec
open assembler.types

let ws = spaces // skips any whitespace

let str s = pstring s
let str_ws s = pstring s >>. ws

let pComment = str_ws "//" >>. skipRestOfLine true

let pSpacing   = // literal translation:
                 //  skipManyChars (pSpace <|> pComment)
                 // more efficient:
                     skipSepBy spaces pComment
let pJGT = str "JGT" >>. pSpacing |>> function _ -> JGT
let pJEQ = str "JEQ" >>. pSpacing |>> function _ -> JEQ
let pJGE = str "JGE" >>. pSpacing |>> function _ -> JGE
let pJLT = str "JLT" >>. pSpacing |>> function _ -> JLT
let pJNE = str "JNE" >>. pSpacing |>> function _ -> JNE
let pJLE = str "JLE" >>. pSpacing |>> function _ -> JLE
let pJMP = str "JMP" >>. pSpacing |>> function _ -> JMP
let pJump = choice [pJGT; pJEQ; pJGE; pJLT; pJNE; pJLE; pJMP]

let runpComment s = run pComment s