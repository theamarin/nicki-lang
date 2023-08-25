# nicki-lang
An experimental hand-written programming language

## How to write your own programming language
Are you interested in how programming languages work?
Then you should absolutely listen to [Immo Landwerth](https://github.com/terrajobst) [writing a basic expression evaluator](https://www.youtube.com/playlist?list=PLRAdsfhKI4OWNOSfS7EUu5GRAVmze1t2y) in less than two hours.

## Written in nim
nicki-lang is implemented in [nim](https://nim-lang.org/).
nim still has a few flaws (mainly [not supporting cyclic symbol dependencies](https://github.com/nim-lang/RFCs/issues/6) as of version 2.0).
For me it still was the most convenient language to write a compact, fast and cross-platform interpreter and compiler.

## How to run
Do you have [nim installed](https://nim-lang.org/)?
Then use
```bash
nim r src/nii.nim
```
to start the interpreter (REPL).
The compiler which should compile nicki-lang to C is not yet functional.
