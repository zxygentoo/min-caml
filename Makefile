TestDIR = test
DuneDIR = _build/default

SRC = \
adder.ml \
cls-bug.ml \
funcomp.ml \
manyargs.ml \
toomanyargs.ml
# ack.ml
# cls-bug2.ml
# cls-rec.ml
# cls-reg-bug.ml
# even-odd.ml
# fib.ml
# float.ml
# gcd.ml
# inprod-loop.ml
# inprod.ml
# inprod-rec.ml
# join-reg2.ml
# join-reg.ml
# join-stack2.ml
# join-stack3.ml
# join-stack.ml
# matmul-flat.ml
# matmul.ml
# non-tail-if2.ml
# non-tail-if.ml
# print.ml
# shuffle.ml
# spill2.ml
# spill3.ml
# spill.ml
# sum.ml
# sum-tail.ml


TESTS   = $(addprefix $(TestDIR)/,$(SRC))
WATS    = $(TESTS:.ml=.wat)
WASMS   = $(TESTS:.ml=.wasm)
RES 	= $(TESTS:.ml=.res)
ANSWERS = $(TESTS:.ml=.answer)
CMP 	= $(TESTS:.ml=.cmp)


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
