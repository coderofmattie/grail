#! /bin/sh

# include macports headers if they are present
# TODO ? what about libs ?
emacs_cflags=""
if test -d /opt/local/include ; then
  emacs_cflags="-I/opt/local/include/"
fi

# configure with X, no fancy toolkit crap, straight X.  It is more
# portable.

conf="./configure --with-x --with-x-toolkit=lucid --without-xaw3d --without-toolkit-scrollbars"

# get rid of more weird crap
conf="$conf --without-pop --without-sound --without-dbus"

# font rendering is critical
conf="$conf --with-xft --with-freetype"

# no funky mouse daemon
conf="$conf --without-gpm"

# graphics formats
conf="$conf --with-png --without-gif --without-jpeg --without-tiff"

# under darwin X is found in /usr/
if test -d /usr/X11/include ; then
  conf="$conf --x-includes=\"/usr/X11/include/\" --x-libraries=\"/usr/X11/lib/\""
fi

conf="$conf --prefix=\"$HOME/system/installed/\""
cd emacs

# putting the system X11R6 include path in the CFLAGS variable, and
# putting the macports include as the x includes path allows macports
# x11 packages to supersede the system packages.

eval exec env CFLAGS="$emacs_cflags"  $conf
