# ocaml-ed
- The beginning of an implementation of `ed` in OCaml

## Architecture
- Main program loop and entry point are found in `oed.ml`
- `editor.ml` stores functions that deal with manipulating the editor's contents
  and settings
- `fileBuffer.ml` contains functions that manipulate the text stored within the
  editor
- `edParser.ml` parses command line commands
- `types.ml` contains some types that are public between all files and is meant
  to be opened

## Dependencies
- OCaml 4.03.0
- re2
- core
- The following commands should install all of the necessary things using `opam`

```
opam switch 4.03.0
opam install re2 core
```

## Compiling
Just run `make` to compile and make the executable `oed.byte`
