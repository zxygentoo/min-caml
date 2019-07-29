TESTSDIR = tests
BUILDDIR = _build/default/src

SOURCES = ack.ml adder.ml cls-bug2.ml cls-bug.ml cls-rec.ml cls-reg-bug.ml \
even-odd.ml fib.ml funcomp.ml gcd.ml inprod-loop.ml inprod.ml inprod-rec.ml \
join-reg2.ml join-reg.ml join-stack2.ml join-stack3.ml join-stack.ml \
manyargs.ml matmul-flat.ml matmul.ml non-tail-if2.ml non-tail-if.ml print.ml \
shuffle.ml spill2.ml spill3.ml spill.ml sum.ml sum-tail.ml toomanyargs.ml

.PRECIOUS %.wat

TESTS   = $(addprefix $(TESTSDIR)/,$(SOURCES))
WATS    = $(TESTS:.ml=.wat)
RESULTS = $(TESTS:.ml=.result)
ANSWERS = $(TESTS:.ml=.answer)
DIFFS   = $(TESTS:.ml=.diff)

all : mincaml
	cp $(BUILDDIR)/main.exe mincaml

mincaml :
	dune build src/main.exe

test : clean mincaml $(DIFFS)

%.diff : %.result %.answer
	diff $?

%.result : %.wat
	node runtime.js $< > $@

%.wat : %.ml
	$(BUILDDIR)/main.exe $(<:.ml=)

%.answer : %.ml
	ocaml $< > $@

clean :
	rm -rf mincaml $(TESTSDIR)/*.{wat,result,answer}
	dune clean

.PHONY : clean test mincaml
