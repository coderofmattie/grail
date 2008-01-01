#! /bin/sh

dir="elisp/"
url="http://www.dr-qubit.org/download.php?file=predictive/auto-overlays-0.9.1.tar.gz&counter=auto-overlays.tar.gz"
cd $dir && wget -O - $url | tar xzf -
