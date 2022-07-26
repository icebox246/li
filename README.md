# li

Simple staticly typed language written in ocaml.

## Examples

Basic test of features:
[examples in test.li](examples/test.li)

It's probably [Turing complete](https://en.wikipedia.org/wiki/Turing_completeness),
because I was able to implement a Turing complete cellular automaton [Rule 110](https://en.wikipedia.org/wiki/Rule_110): 
[rule110.li](examples/rule110.li)

If you ever need to invert a binary tree, such task can be acomplished using this language: [bintree.li](examples/bintree.li)


## Get started

```console
ocaml li.ml examples/test.li
```

<!-- TODOS -->

**TODO (3):**

- [ ] add builtin to read a file
- [ ] make it compilable (to wasm?)
- [ ] maybe native build with nasm

**DONE (24):**

- [x] add early return
- [x] add while loops
- [x] make it so functions can access only global scope vars
- [x] add static typechecking
- [x] prove Turing completeness
- [x] add builtin to read line from stdin
- [x] add character to string conversion
- [x] add character literals
- [x] add comments
- [x] add syntax highlighting for vim
- [x] add builtins for string manipulation
- [x] create function like abstraction for builtins
- [x] add operators for int comparison
- [x] change function definition syntax to be different than variables
- [x] add functions
- [x] add procedures
- [x] add custom tokenization instead of words
- [x] add if-else statements
- [x] add if statements
- [x] add scopes
- [x] add variable scoping
- [x] implement custom parsing
- [x] implement some sort of vm in ocaml
- [x] implement basic parsing
<!-- ENDTODOS -->
