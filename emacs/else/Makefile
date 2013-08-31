.PHONY: clean regress-perl5

# this is some really cute crap. Evidently there is something borked with
# -batch , international is not part of the built-in load-path. Deal with it in a hackish way.
# a better hackish way is to autoconf this thing so I can deal with port issues comprehensively.

# BROKEN: currently getting undef errors. prob some bytecode compat issue.

EMACSLOADPATH = "/usr/share/emacs/21.2/lisp/international/:${HOME}/system/emacs/elisp/"

perl5.lse: perl5.xml
	xsltproc language.xslt $< | sed -e '/^\s*$$/d' >$@

perl5.esl: perl5.lse
	env EMACSLOADPATH=${EMACSLOADPATH} emacs -batch -q -l mule-util -l else-mode --eval '(else-compile-fast-load "$<")'

regress-perl5:
	./assemble perl5 regress.xml >verify.lse
	diff -U3 regress-perl5.lse verify.lse

clean:
	rm -f verify.lse
