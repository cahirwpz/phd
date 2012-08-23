all: spadil.native

tests = $(wildcard tests/*.lisp)

spadil.native:
	ocamlbuild $@

spadil.byte:
	ocamlbuild -lflag -g -cflag -g $@

test: spadil.native
	@for test in $(tests); do echo "*** $${test}"; ./$^ $${test}; done

clean:
	ocamlbuild -clean spadil.{byte,native}
	rm -f *~

.PHONY: spadil.byte spadil.native test clean

# vim: ts=8 sw=8
