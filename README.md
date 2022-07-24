# li

Simple language written in ocaml.

## Example

```
print "Hello," + " World!"

var a := 2
var b := 1 + 3

var c := a * b

c *= 3

print c
```

## Get started

```console
ocaml li.ml
```

<!-- TODOS -->

**TODO (3):**

- [ ] add syntax highlighting for vim
- [ ] make it compilable (to wasm?)
- [ ] maybe native build with nasm

**DONE (14):**

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
