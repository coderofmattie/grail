#! /bin/sh

# Darwin uses the old style /usr/X11R6

emacs_cflags=""
if test -d /usr/X11R6/include ; then
  emacs_cflags="-I/usr/X11R6/include/"
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

# Darwin X11R6 does not have enough headers for a decent compile. The
# only way I get it to work is to try and install GTK from macports
# which pulls all the X headers. Then use the macports includes.

if test -d /opt/local/include ; then
  conf="$conf --x-includes=\"/opt/local/include\""

  # not a good idea really. don't assume pkg-config is broken.
#  export PKG_CONFIG_PATH="/opt/local/lib/pkgconfig/"
fi

conf="$conf --prefix=\"$HOME/system/installed/\""
cd emacs

# putting the system X11R6 include path in the CFLAGS variable, and
# putting the macports include as the x includes path allows macports
# x11 packages to supersede the system packages.

eval exec env CFLAGS="$emacs_cflags"  $conf
