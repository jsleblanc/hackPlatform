# HACK Platform

My implementation of the software projects that are part of the Nand-to-tetris course https://www.nand2tetris.org/

## Projects

| Name      | Description                                                                                                                                                       |
|-----------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| assembler | translates HACK CPU assembly into HACK binary code                                                                                                                |
| vmil2asm  | translates HACK Virtual Machine Intermediate Language (vmil) code into assembly code, including the implementation of the HACK Virtual Machine (also in assembly) |
| jackc     | compiles Jack programming language code into HACK Virtual Machine Intermediate Language                                                                           |
| hackemu   | library containing my own implementation of an emulator for the HACK CPU; used in testing to verify compiled code works correctly                                 |

### Notes

Parsing was implemented using [FParsec](https://www.quanttec.com/fparsec/)
Command line arguments are parsed using [Argu](https://fsprojects.github.io/Argu/)

