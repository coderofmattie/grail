#! /bin/sh

URL="http://luaforge.net/frs/download.php/2724/"
FILE="lua-mode-20071122.tar.gz"

test -f $FILE || wget "$URL$FILE"
tar xzf "$FILE" -C elisp && rm $FILE
