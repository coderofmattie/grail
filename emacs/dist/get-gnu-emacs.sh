#! /bin/sh

if test -d emacs ; then
  cd emacs && cvs update -dP
else
  cvs -z3 -d:pserver:anonymous@cvs.savannah.gnu.org:/sources/emacs co emacs
fi

