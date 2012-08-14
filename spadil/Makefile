all: spadil.native

tests = $(wildcard tests/*.lisp)

spadil.native:
	ocamlbuild $@

test: spadil.native
	@for test in $(tests); do echo $${test}; ./$^ $${test}; done

clean:
	rm -rf _build
	rm -f spadil.native *~

.PHONY: spadil.native test clean

# vim: ts=8 sw=8
