#! /bin/bash

#----------------------------------------------------------------------
# byte compile the elisp files for faster loading.
#----------------------------------------------------------------------

GENERATED="Makefile.bytecode"

if test -f $GENERATED ; then
  if cat <(find compiler.sh -newermm $GENERATED -print) <(find dist/elisp local/elisp local/styles -type d -newermm $GENERATED -print) | xargs test -z ; then
    exit 1
  fi
fi

echo "Regenerating the Makefile"

# the specific emacs to use can be specified.

EMACS=${1:-emacs}

source_dirs="dist/elisp local/elisp local/styles"

recursive=$(find $source_dirs -iname '*.el' -print | tr -s $'\n' ' ')
config=$(ls *.el | tr -s $'\n' ' ')

files="$recursive $config"

# the rule attempts to byte-compile the file, or failing that it deletes the attempt
# to compile, which will slow loading, but not terminate the compile completely

cat >$GENERATED <<RULES
.PHONY: compile clean

.el.elc:
	-\$(EMACS) -batch -f batch-byte-compile $< || rm \$@

RULES

for dep in $files ; do
  base=${dep%.el}

  source="${base}.el"
  target="${base}.elc"

  echo "${target}: ${source}" >>$GENERATED
done

# make sure there is trailing space so that the sed expr matches on the last line.
target_list=$(echo "$files" | sed -e 's,\.el,\.elc,g')


cat >>$GENERATED <<COMMANDS
compile: $target_list
clean:
	-rm 2>/dev/null $target_list
COMMANDS

exit 0
