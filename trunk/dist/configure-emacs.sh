#! /bin/sh

cd emacs

exec ./configure --with-x --prefix=$HOME/system/local/ \
--with-xft \
--without-gpm \
--without-carbon \
--without-xaw3d \
--without-toolkit-scrollbars \
--without-gif \
--without-png \
--without-jpeg \
--without-tiff \
--with-x-toolkit=lucid \

