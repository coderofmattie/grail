#! /bin/sh

unset EMACSLOADPATH
cd emacs

# prevent auto-load hell
(cd lisp && make autogen-clean && make autoloads)
make bootstrap
