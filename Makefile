TESTSDIR = tests
BUILDDIR = _build/default

SOURCES = \
print.ml \
adder.ml \
cls-bug.ml \
cls-rec.ml \
funcomp.ml \
manyargs.ml \
toomanyargs.ml \
ack.ml \
even-odd.ml \
fib.ml \
gcd.ml \
join-reg.ml \
join-reg2.ml \
join-stack.ml \
join-stack2.ml \
join-stack3.ml \
shuffle.ml \
sum.ml \
sum-tail.ml \
spill.ml \
spill2.ml \
spill3.ml \
cls-reg-bug.ml \
non-tail-if.ml \
inprod.ml \
# --------- FAIL TESTS -----------
# cls-bug2.ml \
# inprod-rec.ml \
# inprod-loop.ml \
# non-tail-if2.ml \
# matmul.ml \
# matmul-flat.ml \
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
