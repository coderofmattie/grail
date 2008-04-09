#! /bin/sh

cd emacs

# putting the system X11R6 include path in the CFLAGS variable, and
# putting the macports include as the x includes path allows macports
# x11 packages to supersede the system packages.

exec env CFLAGS="-I/usr/X11R6/include/" \
./configure --with-x --prefix=$HOME/system/local/ \
--x-includes="/opt/local/include" \
--with-xft \
--with-freetype \
--without-gpm \
--without-carbon \
--without-xaw3d \
--without-toolkit-scrollbars \
--without-gif \
--without-png \
--without-jpeg \
--without-tiff \
--with-x-toolkit=lucid \

