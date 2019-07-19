TestDIR = test
DuneDIR = _build/default

SRC = adder.ml \
	cls-bug.ml \
	funcomp.ml \
	manyargs.ml \
	toomanyargs.ml

SOURCES  = $(addprefix $(TestDIR)/,$(SRC))
WATS     = $(SOURCES:.ml=.wat)
WASMS    = $(SOURCES:.ml=.wasm)
RES 	 = $(SOURCES:.ml=.res)
ANSWERS   = $(SOURCES:.ml=.answer)
CMP 	 = $(SOURCES:.ml=.cmp)


test : clean mincaml $(CMP)

adder :
	ocaml test/adder.ml > test/adder.answer
	_build/default/main.exe test/adder
	wat2wasm test/adder.wat -o test/adder.wasm
	node run.js test/adder.wasm > test/adder.res
	diff test/adder.answer test/adder.res

mincaml :
	dune build main.exe

%.cmp : %.res %.answer
	diff $?

%.res : %.wasm
	node run.js $< > $@

%.answer : %.ml
	ocaml $< > $@

%.wasm : %.wat
	wat2wasm $< -o $@

%.wat : %.ml
	$(DuneDIR)/main.exe $(<:.ml=)

clean :
	rm -rf $(WATS) $(WASMS) $(ANSWERS) $(RES)

.PHONY : clean
