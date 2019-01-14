SOURCES = type.ml id.ml m.ml s.ml syntax.ml parser.mli parser.ml lexer.ml typing.mli typing.ml asm.mli asm.ml knormal.mli knormal.ml \
		alpha.mli alpha.ml beta.mli beta.ml assoc.mli assoc.ml inline.mli inline.ml constfold.mli constfold.ml elim.mli elim.ml \
		closure.mli closure.ml virtual.mli virtual.ml simm.mli simm.ml regalloc.mli regalloc.ml emit.mli emit.ml main.mli main.ml
MODULES = type id m s syntax parser lexer typing asm knormal alpha beta assoc inline constfold elim closure virtual simm regalloc emit main

all : comp top

comp : lexer_and_parser float.o
	ocamlfind opt -c $(SOURCES)
	ocamlfind opt -o comp float.o $(MODULES:=.cmx)

top : lexer_and_parser float.o 
	ocamlfind ocamlc -c $(SOURCES)
	ocamlmktop -custom -warn-error -31 -o top float.o $(MODULES:=.cmo)

lexer_and_parser : 
	ocamlyacc parser.mly
	ocamllex lexer.mll

float.o : float.c
	$(CC) -Wall -c -o $@ $<

clean :
	rm -f *.cmi *.cmx *.cmo *.o parser.mli parser.ml lexer.ml top comp
