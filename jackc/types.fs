module jackc.types

type JackTypes =
    | J_Int
    | J_Char
    | J_Boolean
    | J_Class of string
    
type JackClassVariableScope =
    | Static
    | Field
    
type JackLang =
    | J_ClassVariableDeclaration of JackClassVariableScope * JackTypes * string list