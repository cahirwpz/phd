all: load.input

load.input: compile.input
	rm -f $@
	for spad in `cut -f 2 -d ' ' $<`; do			\
	  for lib in `grep '^)abbrev' $$spad | cut -f 3 -d ' '`; do	\
	    echo ")library $$lib" >> $@;				\
	  done								\
	done

clean:
	rm -f load.input *.fasl *~
	rm -rf *.NRLIB
	rm -rf *.erlib

.PHONY: all clean

# vim: ts=8 sw=8 noet
