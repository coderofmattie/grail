#! /bin/sh

cd emacs

# putting the system X11R6 include path in the CFLAGS variable, and
# putting the macports include as the x includes path allows macports
# x11 packages to supersede the system packages.

exec env CFLAGS="-I/usr/X11R6/include/" \
./configure \
\
--with-x \
--with-x-toolkit=lucid \
--x-includes="/opt/local/include" \
--with-xft \
--with-freetype \
\
--without-gpm \
--without-carbon \
--without-xaw3d \
--without-toolkit-scrollbars \
\
--with-png \
--without-gif \
--without-jpeg \
--without-tiff \
\
--prefix="$HOME/system/local/"
