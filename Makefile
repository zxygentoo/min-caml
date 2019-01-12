SOURCES = type.ml id.ml m.ml s.ml syntax.ml parser.mli parser.ml lexer.ml typing.mli typing.ml asm.mli asm.ml kNormal.mli kNormal.ml \
		alpha.mli alpha.ml beta.mli beta.ml assoc.mli assoc.ml inline.mli inline.ml constFold.mli constFold.ml elim.mli elim.ml \
		closure.mli closure.ml virtual.mli virtual.ml simm.mli simm.ml regAlloc.mli regAlloc.ml emit.mli emit.ml main.mli main.ml
MODULES = type id m s syntax parser lexer typing asm kNormal alpha beta assoc inline constFold elim closure virtual simm regAlloc emit main

all : comp top

comp : parser.mli parser.ml lexer.ml float.o
	ocamlfind opt -c $(SOURCES)
	ocamlfind opt -o comp float.o $(MODULES:=.cmx)

top : parser.ml parser.mli lexer.ml float.o
	ocamlfind ocamlc -c $(SOURCES)
	ocamlmktop float.o -custom  -warn-error -31 -o top $(MODULES:=.cmo)

parser.mli parser.ml : parser.mly
	ocamlyacc $<

lexer.ml : lexer.mll
	ocamllex $<

float.o : float.c
	$(CC) -Wall -c -o $@ $<

clean :
	rm -f *.cmi *.cmx *.cmo *.o parser.mli parser.ml lexer.ml top comp
