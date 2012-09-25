CC	 = clang
CPPFLAGS = -I/opt/local/include
CFLAGS	 = -fno-inline -O2

FLAGS	= -use-ocamlfind -cflag -g -lflag -g

all: spadil.native

tests = $(wildcard tests/*.lisp)

spadil.native: runtime.bc vmdata.bc
	ocamlbuild $(FLAGS) $@

spadil.byte: runtime.bc vmdata.bc
	ocamlbuild $(FLAGS) $@

%.bc: %.c
	$(CC) $(CPPFLAGS) -emit-llvm $(CFLAGS) -o $@ -c $^

%.ll: %.bc
	llvm-dis $^

test: spadil.native
	@for test in $(tests); do echo "*** $${test}"; ./$^ $${test}; done

clean:
	ocamlbuild -clean spadil.{byte,native}
	@find . -follow -iname '*~' | xargs rm
	@rm -vf *.bc *.ll

.PHONY: spadil.byte spadil.native test clean

# vim: ts=8 sw=8
