#----------------------------------------------------------------------
#
#----------------------------------------------------------------------
.PHONY: clean library grail config dist dist-clean all
.DEFAULT: all

.el.elc:
	emacs -q -L . -L local/elisp/ --batch -f batch-byte-compile $<
clean:
	cd local/elisp && make clean
	- rm *.elc

library:
	cd local/elisp && make all

grail-fn.elc: grail-fn.el
grail-groups.elc: grail-groups.el

grail: grail-fn.elc grail-groups.elc

user.elc: user.el
programming.elc: programming.el
elisp.elc: elisp.el
keys.elc: keys.el
beta.elc: beta.el
commands.elc: commands.el

config: user.elc programming.elc elisp.elc keys.elc beta.elc

dist-clean:
	- find dist/elisp -name '*.elc' | xargs rm

dist:
	find dist/elisp -name '*.el' | xargs emacs -q --batch -f batch-byte-compile

all: library grail config
