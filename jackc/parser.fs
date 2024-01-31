module jackc.parser

open FParsec

let ws = spaces // skips any whitespace
let str s = pstring s

//let pCommentSingleLine = str "//" >>. ws >>. restOfLine true //|>> function c -> Comment c
