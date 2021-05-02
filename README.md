# fscala2c
 [![SBT test](https://github.com/Linyxus/fscala2c/actions/workflows/ci.yml/badge.svg)](https://github.com/Linyxus/fscala2c/actions/workflows/ci.yml)

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
 
 - [X] Tokenizer (:construction: being refactored)
 
 - [X] packratc: Packrat Parser Combinator library (:construction: being refactored)
 
 - [X] Parser: tokens --> Featherweight Scala raw AST (:construction: being refactored)
 
 - [X] Typer: Raw AST --> Typed AST (Scala Core IR)
   - [X] Local type inference for expressions
   - [X] Hindley-Milner type inference for recursive definition group in block expressions
   - [X] Hindley-Milner type inference for recursive class definition
 
 - [ ] **(CANCELLED)** Optimize typed AST (optimization is so boring; let's do this later)
 
 - [X] Typed AST --> C AST (C Core IR)
 
 - [ ] **(CANCELLED)** Optimize C AST
 
 - [X] C AST --> C source code
 
 ## Examples
 
 ### Fibonacci Array (CPS)
 
 We will compile the following FScala code to C:
 ```scala
 class Main {
   val fibonacci = (n: Int, callback: Int => Int) =>
     if n <= 1 then
       callback(1)
     else
       fibonacci(n - 1, (t1: Int) => fibonacci(n - 2, (t2: Int) => callback(t1 + t2)))

   val identity = (x: Int) => x

   val main = () => {
     val n = readInt()
     val res = fibonacci(n, identity)
     printlnInt(res)
   }
 }
 ```
 Note that the above example uses continuation-passing-style (CPS) to compute the Fibonacci array. The CPS style utilizes first-class functions (lambda literals and passing functions just like any other value), which is typically not very usable or even not available in low-level languages like C.
 
 Assuming that the file is located at `fibo_cps.scala`, We can compile it to C with:
 ```shell
 sbt 'run --source fibo_cps.scala --output out/fibo_cps.c'
 ```
 
 The compiled the file could be found at `out/fibo_cps.c`. Compile it with `clang`:
 ```shell
 clang out/fibo_cps.c -O3 -o out/fibo_cps
 ```
 
 And run it:
 ```shell
 $ ./out/fibo_cps
 10
 89
 ```
 
 Voila! The Scala code is compiled to C and runs smoothly in machine code!

 ## References

 - Hindley-Milner type inference for recursive definition groups:
   [Wikipedia](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system),
   [Blog](https://blog.linyxus.xyz/posts/hm-type-inference-for-stlc/)

 - Closure conversion for compiling lambda expressions to global definitions:
   [Slides](http://lampwww.epfl.ch/teaching/archive/advanced_compiler/2007/resources/slides/act-2007-05-closure-conversion.pdf),
   [Blog](http://matt.might.net/articles/closure-conversion/),
   [Tutorial](https://craftinginterpreters.com/closures.html)
