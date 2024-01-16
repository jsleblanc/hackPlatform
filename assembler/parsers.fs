module assembler.parsers

open FParsec

let parseComment s = pstring "//" >>. pstring s .>> newline