.PHONY: all test clean

version := $(shell swipl -q -s pack -g 'version(V),writeln(V)' -t halt)
packfile = quickcheck-$(version).tgz

SWIPL := swipl

all: test

version:
	echo $(version)

check: test

install:
	echo "(none)"

test:
	@$(SWIPL) -s test/test.pl -g 'run_tests,halt(0)' -t 'halt(1)'
