# min-caml-wasm

A [min-caml](https://github.com/esumii/min-caml) port targeting [WebAssembly](https://webassembly.org).

## Dependencies

- [OCaml](http://ocaml.org/) compiler *(tested on 4.7.0)*
- recent release of [NodeJs](https://nodejs.org/) with WebAssembly support *(tested on v8.12.0)*

### OCaml dependencies

- using [opam](http://opam.ocaml.org):

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

- [wabt](https://www.npmjs.com/package/wabt): for parsing emitted ```.wat``` file to WebAssembly module.

## Usage

### Build the compiler

```
make
```

### Compile source file

```
./mincaml tests/print        # filename without ".ml"
```

*The original **min-caml** compiler requires filename without ```.ml```. I decided not to change it.*

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

## Differences to min-caml

- Compilation pipeline: WebAssembly is rather a "high-level" (compare to assembly) compilation target. There isn't much to do for virtual code preparation and register allocation, so after closure conversion (```closure.ml(i)```) we go direct to code emission (```emit.ml(i)```).
- K-Normalization: WebAssembly ```if...then...else``` operation is typed, so changes had to be made to ```knormal.ml(i)``` to pass the result type of ```IfEq``` and ```IfLe```.


## TODO

- port min-rt
- maybe a Virtual.ml after all to make things nicer
