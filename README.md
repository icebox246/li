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

**TODO (6):**

- [ ] add variable scoping
- [ ] add procedures
- [ ] add functions
- [ ] add custom tokenization instead of words
- [ ] make it compilable (to wasm?)
- [ ] maybe native build with nasm

**DONE (2):**

- [x] implement some sort of vm in ocaml
- [x] implement basic parsing
<!-- ENDTODOS -->
