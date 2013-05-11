all: spad.native

spad.native:
	ocamlbuild $@

spad.byte:
	ocamlbuild -lflag -g -cflag -g $@

clean:
	ocamlbuild -clean spad.{byte,native}
	@find . -follow -iname '*~' | xargs rm

.PHONY: spad.byte spad.native test clean

# vim: ts=8 sw=8
