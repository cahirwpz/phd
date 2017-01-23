AXIOM = $$FRICAS_PREFIX/lib/fricas/target/x86_64-unknown-linux
TESTS = Test1.spad \
	Test2.spad \
	Test3.spad \
	Test4.spad \
	Test5.spad \
	Test6.spad \
	Test7.spad \
	Test8.spad \
	Test9.spad \
	Test10.spad \
	Test11.spad

all: build load.input

load.input: compile.input
	rm -f $@
	for spad in `cut -f 2 -d ' ' $<`; do			\
	  for lib in `grep '^)abbrev' $$spad | cut -f 3 -d ' '`; do	\
	    echo ")library $$lib" >> $@;				\
	    if [ -d $$lib-.NRLIB ]; then				\
	      echo ")library $$lib-" >> $@;				\
	    fi								\
	  done								\
	done

build: compile.input
	./axiom-run --read $<

check: load.input 
	for test in $(TESTS); do					\
	  ./axiom-run --compile $$test;					\
	done

clean:
	rm -f load.input *.clisp *.fasl *~
	rm -rf *.NRLIB
	rm -rf *.erlib

.PHONY: all build clean

# vim: ts=8 sw=8 noet
