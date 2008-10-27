#! /bin/sh

URL="http://yasnippet.googlecode.com/files/"
VERSION="0.5.6"
FILE="yasnippet-$VERSION.tar.bz2"

TARGET="elisp/yasnippet/yasnippet.el"

if test -f $TARGET ; then
  echo "nothing to be done ... delete $TARGET"
  exit 1
fi

test -f $FILE || wget "$URL$FILE"

cd elisp

tar xjf "../yasnippet-$VERSION.tar.bz2" "yasnippet-$VERSION/yasnippet.el"

if ! test -d "yasnippet-$VERSION" ; then
  echo "failure to unpack $FILE"
  exit 1
fi

mv yasnippet{-$VERSION,}



