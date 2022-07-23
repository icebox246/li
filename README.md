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

**TODO (4):**

- [ ] add procedures
- [ ] add functions
- [ ] make it compilable (to wasm?)
- [ ] maybe native build with nasm

**DONE (8):**

- [x] add custom tokenization instead of words
- [x] add if-else statements
- [x] add if statements
- [x] add scopes
- [x] add variable scoping
- [x] implement custom parsing
- [x] implement some sort of vm in ocaml
- [x] implement basic parsing
<!-- ENDTODOS -->
