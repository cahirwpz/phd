FLAGS = -use-ocamlfind

all: spadil.native

tests = $(wildcard tests/*.lisp)

spadil.native: runtime.bc
	ocamlbuild $(FLAGS) $@

spadil.byte: runtime.bc
	ocamlbuild $(FLAGS) -lflag -g -cflag -g $@

runtime.bc: runtime.c
	clang -emit-llvm -O2 -o $@ -c $^

test: spadil.native
	@for test in $(tests); do echo "*** $${test}"; ./$^ $${test}; done

clean:
	ocamlbuild -clean spadil.{byte,native}
	@find . -follow -iname '*~' | xargs rm
	@rm -vf "runtime.bc"

.PHONY: spadil.byte spadil.native test clean

# vim: ts=8 sw=8
