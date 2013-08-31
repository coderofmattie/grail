#! /bin/bash

#----------------------------------------------------------------------
# byte compile the elisp files for faster loading.
#----------------------------------------------------------------------

# This script generates a Makefile to compile the elisp files added by
# the user (source_dirs).

GENERATED="Makefile.bytecode"
source_dirs="dist/elisp local/elisp local/styles"


# skip re-generating if the makefile is newer than the directories in
# the source_dir list and it is newer than this script itself.

if test -f $GENERATED ; then
  if cat <(find compiler.sh -newermm $GENERATED -print) <(find dist/elisp local/elisp local/styles -type d -newermm $GENERATED -print) | xargs test -z ; then
    exit 1
  fi
fi

echo "Regenerating the Makefile"

# the specific emacs to use can be specified.

EMACS=${1:-emacs}


# find all of the elisp files to be compiled. the base directory of the config
# is handled as a special case to limit the recursion to source_dirs

recursive=$(find $source_dirs -iname '*.el' -print | tr -s $'\n' ' ')
config=$(ls *.el | tr -s $'\n' ' ')

files="$recursive $config"

# Create the rule to byte-compile an elisp file. If the byte compile
# failes ensure that it is deleted so that the source will be loaded.

# ignore compile failures.

cat >$GENERATED <<RULES
.PHONY: compile clean

.el.elc:
	-\$(EMACS) -batch -f batch-byte-compile $< || rm \$@

RULES

# write the dependencies

for dep in $files ; do
  base=${dep%.el}

  source="${base}.el"
  target="${base}.elc"

  echo "${target}: ${source}" >>$GENERATED
done

target_list=$(echo "$files" | sed -e 's,\.el,\.elc,g')


# create the top-level targets

cat >>$GENERATED <<COMMANDS
compile: $target_list
clean:
	-rm 2>/dev/null $target_list
COMMANDS

exit 0
