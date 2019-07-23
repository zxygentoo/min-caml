TESTSDIR = tests
BUILDDIR = _build/default

SOURCES = \
ack.ml \
adder.ml \
cls-bug2.ml \
cls-bug.ml \
cls-rec.ml \
cls-reg-bug.ml \
even-odd.ml \
fib.ml \
funcomp.ml \
gcd.ml \
inprod-loop.ml \
inprod.ml \
inprod-rec.ml \
join-reg2.ml \
join-reg.ml \
join-stack2.ml \
join-stack3.ml \
join-stack.ml \
manyargs.ml \
matmul-flat.ml \
non-tail-if2.ml \
non-tail-if.ml \
print.ml \
shuffle.ml \
spill2.ml \
spill3.ml \
spill.ml \
sum.ml \
sum-tail.ml \
toomanyargs.ml \
# --------- FAIL TESTS -----------
# matmul.ml \
# float.ml \


TESTS   = $(addprefix $(TESTSDIR)/,$(SOURCES))
WATS    = $(TESTS:.ml=.wat)
WASMS   = $(TESTS:.ml=.wasm)
RESULTS = $(TESTS:.ml=.result)
ANSWERS = $(TESTS:.ml=.answer)
DIFFS   = $(TESTS:.ml=.diff)


mincaml :
	dune build main.exe

test : clean mincaml $(DIFFS)

%.diff : %.result %.answer
	diff $?

%.result : %.wasm
	node runtime.js $< > $@

%.wasm : %.wat
	wat2wasm $< -o $@

%.wat : %.ml
	$(BUILDDIR)/main.exe $(<:.ml=)

%.answer : %.ml
	ocaml $< > $@

clean :
	rm -rf $(WATS) $(WASMS) $(ANSWERS) $(RESULTS) mincaml
	dune clean

.PHONY : clean test mincaml
