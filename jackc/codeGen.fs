module jackc.codeGen

open jackc.types
open jackc.symbolTable
open jackc.validation

let compileClass (c:JackClass) =
    let classSymbols = buildSymbolsForClass c
    
    errorMsg c.name "not implemented yet"