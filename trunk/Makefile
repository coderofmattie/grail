
#----------------------------------------------------------------------
# Makfile: for the emacs configuration
#----------------------------------------------------------------------

EMACS=$(HOME)/system/bin/emacs

BYTECODE="Makefile.bytecode"

.PHONY: all $(BYTECODE) compile clean

lisp-fonts:
	test -f $HOME/.fonts.conf && fc-cache -f

$(BYTECODE):
	-./compiler.sh

compile: $(BYTECODE)
	${MAKE} -f Makefile.bytecode compile EMACS=$(EMACS)

clean: $(BYTECODE)
	${MAKE} -f Makefile.bytecode clean

all: fonts


