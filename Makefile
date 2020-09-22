.PHONY: all test clean

all:
	@dune build
	@ln -f -s _build/default/bin/gene.exe gene.exe

test:
	@dune runtest

clean:
	@dune clean
	@rm -f gene.exe
