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
spill3.ml \
# --------- FAIL TESTS -----------
# cls-reg-bug.ml \
# cls-bug2.ml \
# spill2.ml \
# non-tail-if.ml \
# non-tail-if2.ml \
# inprod.ml \
# inprod-loop.ml \
# inprod-rec.ml \
# float.ml \
# matmul.ml \
# matmul-flat.ml \


TESTS   = $(addprefix $(TESTSDIR)/,$(SOURCES))
WATS    = $(TESTS:.ml=.wat)
WASMS   = $(TESTS:.ml=.wasm)
RESULTS = $(TESTS:.ml=.result)
ANSWERS = $(TESTS:.ml=.answer)
DIFFS   = $(TESTS:.ml=.diff)


all : mincaml

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
	rm -rf $(WATS) $(WASMS) $(ANSWERS) $(RESULTS)
  dune clean

.PHONY : clean test mincaml