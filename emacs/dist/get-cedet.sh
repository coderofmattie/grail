#! /bin/sh

cd elisp

if test -d cedet ; then
  cd cedet && cvs update
  touch `find -name Makefile -print`
  make
else
  cvs -d:pserver:anonymous@cedet.cvs.sourceforge.net:/cvsroot/cedet login
  cvs -z3 -d:pserver:anonymous@cedet.cvs.sourceforge.net:/cvsroot/cedet co -P cedet
fi
