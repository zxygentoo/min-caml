# min-caml-wasm

A [min-caml](https://github.com/esumii/min-caml) port targeting [WebAssembly](https://webassembly.org).

## Dependencies

- [OCaml](http://ocaml.org/) compiler *(tested on 4.7.0)*
- recent release of [NodeJs](https://nodejs.org/) with WebAssembly support *(tested on v8.12.0)*

### OCaml dependencies

Install using (opam)[http://opam.ocaml.org]:

```
opam switch 4.07.0
opam install dune ppx_jane ppx_deriving
```

- [dune](https://dune.build/): for building the compiler.
- ppx_jane/ppx_deriving: preprocessors for debugging.

### JavaScript dependencies

```
cd min-ocaml-wasm
npm install
```

- [wabt](https://www.npmjs.com/package/wabt): for loading min-caml-wasm emitted .wat file as WebAssembly module.

## Usage

### Build the compiler

```
make
```

### Compile source file

```
./mincaml tests/print        # filename without ".ml"
```

- ```min-caml``` command line require filename without ```.ml```. I decided to keep it, but it still looks werid to me.

### Run compiled module

```
node runtime.js tests/print.wat
```

### Run tests from min-caml project

```
make test
```

### Clean up

```
make clean
```

## Differences with min-caml

- Closure
- KNormal
