# copyright (c) 2014, guillaume bury

FLAGS?=
BINDIR=_build/install/default/bin

all: build

watch:
	dune build $(FLAGS) -w @check

build:
	dune build $(FLAGS) @install

top:
	dune utop

doc:
	dune build $(FLAGS) @doc

test:
	dune build $(FLAGS) @runtest

promote:
	-dune build $(FLAGS) @runtest
	dune promote $(FLAGS)

clean:
	dune clean

.PHONY: all watch dune doc test promote clean
