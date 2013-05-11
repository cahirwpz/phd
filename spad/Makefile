FLAGS = -use-ocamlfind

all: spad.native

spad.native:
	ocamlbuild $(FLAGS) $@

spad.byte:
	ocamlbuild $(FLAGS) -lflag -g -cflag -g $@

clean:
	ocamlbuild -clean spad.{byte,native}
	@find . -follow -iname '*~' | xargs rm

.PHONY: spad.byte spad.native test clean

# vim: ts=8 sw=8
