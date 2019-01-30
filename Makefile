TESTDIR = test
_SOURCES = adder.ml cls-bug.ml funcomp.ml

SOURCES = $(addprefix $(TESTDIR)/,$(_SOURCES))
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
	_build/default/comp.exe $(<:.ml=)

clean :
	rm -rf $(SOURCES:.ml=.wasm) $(SOURCES:.ml=.wat)

.PHONY : clean
