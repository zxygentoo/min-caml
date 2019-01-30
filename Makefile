all :
	dune build comp.exe
	_build/default/comp.exe test/adder
	_build/default/comp.exe test/cls-bug
	_build/default/comp.exe test/funcomp
	node tests.js
