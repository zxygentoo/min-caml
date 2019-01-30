TestDIR = test
DuneDIR = _build/default

SRC = adder.ml cls-bug.ml funcomp.ml

SOURCES = $(addprefix $(TestDIR)/,$(SRC))
WATS = $(SOURCES:.ml=.wat)
WASMS = $(SOURCES:.ml=.wasm)


test : clean compiler $(WATS) $(WASMS) nodetest

compiler :
	dune build comp.exe

nodetest :
	node tests.js

%.wasm : %.wat
	wat2wasm $< -o $@

%.wat : %.ml
	$(DuneDIR)/comp.exe $(<:.ml=)

clean :
	rm -rf $(WATS) $(WASMS)

.PHONY : clean
