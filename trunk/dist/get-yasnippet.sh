#! /bin/sh

cd elisp
if test -d yasnippet ; then
  svn update
else
  svn checkout http://yasnippet.googlecode.com/svn/trunk/ yasnippet;
fi;
