#----------------------------------------------------------------------
#
#----------------------------------------------------------------------
.PHONY: clean library buffer-ring vcs elisp
.DEFAULT: elisp

.el.elc:
	emacs -q -L . -L ../../ --batch -f batch-byte-compile $<
clean:
	- rm *.elc

cm-lisp.elc: cm-lisp.el
cm-list.elc: cm-list.el
cm-string.elc: cm-string.el
cm-util.elc: cm-util.el
cm-path.elc: cm-path.el
cm-compat.elc: cm-compat.el

record-processor.elc: record-processor.el

library: cm-lisp.elc cm-string.elc cm-list.elc cm-util.elc cm-path.elc cm-compat.elc\
 record-processor.elc

buffer-ring.elc: buffer-ring.el
dynamic-ring.elc: dynamic-ring.el

buffer-ring: buffer-ring.elc dynamic-ring.elc

bzr-ui.elc: bzr-ui.el
commit-file.elc: commit-file.el
submerge.elc: submerge.el
working-copy.elc: working-copy.el

vcs: bzr-ui.elc commit-file.elc working-copy.elc

dwim-tab.elc: dwim-tab.el
ucase-word.elc: ucase-word.el
mattie-modeline.elc: mattie-modeline.el
elisp: library buffer-ring vcs ucase-word.elc dwim-tab.elc mattie-modeline.elc

all: elisp
