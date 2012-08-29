all: spaddy.native

spaddy.native:
	ocamlbuild $@

spaddy.byte:
	ocamlbuild -lflag -g -cflag -g $@

clean:
	ocamlbuild -clean spaddy.{byte,native}
	rm -f *~

.PHONY: spaddy.byte spaddy.native test clean

# vim: ts=8 sw=8
