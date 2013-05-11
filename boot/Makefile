FLAGS = -use-ocamlfind

all: boot.native symbols.lst

boot.native:
	ocamlbuild $(FLAGS) $@

boot.byte:
	ocamlbuild $(FLAGS) -lflag -g -cflag -g $@

symbols.lst:
	./symbols.lisp

clean:
	ocamlbuild -clean boot.{byte,native}
	@find . -follow -iname '*~' | xargs rm
	rm symbols.lst

.PHONY: boot.byte boot.native test clean

# vim: ts=8 sw=8
