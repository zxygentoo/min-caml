# min-caml-wasm

A [min-caml](https://github.com/esumii/min-caml) port targeting [WebAssembly](https://webassembly.org).

## Dependencies

### OCaml

```
opam switch 4.07.0
opam install dune ppx_jane ppx_deriving
```

- [dune](https://dune.build/): for building the compiler.
- ppx_jane/ppx_deriving: preprocessors for debuging.

### JavaScript

```
cd min-ocaml-wasm
npm install
```

- [wabt](https://www.npmjs.com/package/wabt): for loading the .wat file min-caml-wasm emitted as WebAssembly module.

## Usage

### Build the compiler

### Compile source file

### Run compiled module


## Differences with min-caml

