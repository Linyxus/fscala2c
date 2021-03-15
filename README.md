# fs2c
 A Featherweight Scala to C compiler.
 
 Compiler course project at BUPT.
 
 Phases:

 - Frontend
   - Tokenizing
   - Parsing
   - Typing
 - Backend
   - Scala Core IR optimizing
   - C Core IR generation
   - C Core IR optimizing
   - C code generation

 ## Roadmap
 
 - [X] Tokenizer
 
 - [X] packratc: Packrat Parser Combinator library
 
 - [X] Parser: tokens --> Featherweight Scala raw AST
 
 - [X] Typer: Raw AST --> Typed AST (Scala Core IR)
   - [X] Local type inference for expressions
   - [X] Hindley-Milner type inference for recursive definition group in block expressions
   - [X] Hindley-Milner type inference for recursive class definition
 
 - [ ] Optimize typed AST (optimization is so boring; let's do this later)
 
 - [ ] Typed AST --> C AST (C Core IR)
 
 - [ ] Optimize C AST
 
 - [ ] C AST --> C source code
