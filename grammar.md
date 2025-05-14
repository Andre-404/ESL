# ESL Grammar
Repetition of a rule is denoted by *

Optional rule(s) are inside [ ]
```
program -> import* declaration* EOF
import -> "import" string ["as" identifier]
declaration -> ["pub"](varDecl | funcDecl | classDecl) | statement
varDecl -> "let" [type] identifier (";" | "=" expr ";")
funcDecl -> "fn" identifier "(" paramlist ")" [type] blockStmt
paramlist -> identifier [type] ("," identifier [type])* | 
classDecl -> "class" identifier [":" declaccess] "{" [ (["pub"](classField | classMethod))* ] "}"
classField -> "let" [type] identifier ("," identifier)*
classMethod -> ["override"] funcDecl

localDeclaration -> varDecl | statement
statement -> blockStmt | ifStmt | whileStmt | forStmt | switchStmt | returnStmt | spawnStmt |
             ("advance" | "break" | "continue" | expr) ";"
blockStmt -> "{" localDeclaration* "}
ifStmt -> "if" "(" expr ")" statement ["else" statement]
whileStmt -> "while" "(" expr ")" statement
forStmt -> "for" "(" [varDecl] ";" [expr] ";" [expr] ")" statement
switchStmt -> "switch" "(" expr ")" "{" case* "}"
case -> "case" expr ":" statement | "default" ":" statement
returnStmt -> "return" expr ";"
spawnStmt -> "spawn" call "(" arglist ")" ";"

expr -> assignment | conditional | or | and | comparison | binor | binxor | binand |
        bitshift | sum | factor | unary | increment | is | call | primary

assignment -> lvalue assignop assignment
assignop -> "=" | "+=" | "-=" | "*=" | "/=" | "|=" | "^=" | "&=" | "%=" |

conditional -> or "?" expr ":" conditional
or -> or "||" and | and
and -> and "&&" equality | equality
equality ->  equality ("==" | "!=") relational | relational
relational -> binor compop binor | binor
compop -> "<" | ">" | "<=" | ">= | "is"

binor -> binor "|" binxor | binxor
binxor -> binxor "^" binand | binand
binand -> binand "&" bitshift | bitshift
bitshift -> bitshift ("<<" | ">>") sum | sum

sum -> sum ("+" | "-") factor | factor
factor -> factor ("*" | "-") unary | unary
unary -> ("!" | "~") unary | ("++" | "--") lvalue | call

call -> ["new"] call "(" arglist ")" | access | primary
arglist -> expr ("," expr)* |
access ->  call "." identifier | call "[" expr "]"

primary -> declaccess | number | string | true | false | null |
           funcDecl | "{" (string ":" expr)* "}" | "(" expr ")"
lvalue -> declaccess | access
declaccess -> identifier "::" identifier | identifier
type -> declaccess

identifier := (_ + [a-zA-z])(_ + [a-zA-Z] + [0-9])*
number := any whole or decimal number
string := any sequence of characters wrapped inside of " "

```