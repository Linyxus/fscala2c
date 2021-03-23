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

 ## References

 - Hindley-Milner type inference for recursive definition groups:
   [Wikipedia](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system),
   [Blog](https://blog.linyxus.xyz/posts/hm-type-inference-for-stlc/)

 - Closure conversion for compiling lambda expressions to global definitions:
   [Slides](http://lampwww.epfl.ch/teaching/archive/advanced_compiler/2007/resources/slides/act-2007-05-closure-conversion.pdf),
   [Blog](http://matt.might.net/articles/closure-conversion/),
   [Tutorial](https://craftinginterpreters.com/closures.html)
