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

all: load.input

load.input: compile.input
	rm -f $@
	for spad in `cut -f 2 -d ' ' $<`; do			\
	  for lib in `grep '^)abbrev' $$spad | cut -f 3 -d ' '`; do	\
	    echo ")library $$lib" >> $@;				\
	  done								\
	done

build: compile.input
	./axiom-run $<

check: load.input 
	for test in $(TESTS); do					\
	  ./axiom-run --test $$test;					\
	done

clean:
	rm -f load.input *.fasl *~
	rm -rf *.NRLIB
	rm -rf *.erlib

.PHONY: all build clean

# vim: ts=8 sw=8 noet
