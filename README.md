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
 
 - [ ] Parser: tokens --> Featherweight Scala raw AST
 
 - [ ] Raw AST --> Typed AST (Scala Core IR)
 
 - [ ] Optimize typed AST
 
 - [ ] Typed AST --> C AST (C Core IR)
 
 - [ ] Optimize C AST
 
 - [ ] C AST --> C source code
