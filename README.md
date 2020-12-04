# Grail configuration for Emacs

An Emacs configuration and loader.

## Early History

Grail began as a Emacs lisp cut and paste mess of config snippets around 2000 or so. I swore that one day I would learn Elisp so I could change the file without breaking it.

It was copied around as a .emacs file and then one day I sat down to learn Emacs and wrote a proper config and eventually checked it into SubVersion.

After a lot of hacking for a while I migrated to git and used it as a local repository.

## Big re-design

When I moved to the big city and started a job that required more power in the Emacs department to compensate for the shitty tooling. I rewrote it into it's current design in three months of round the clock overtime.

The current design is that it "bootstraps" itself by simplifying dropping in the config, making a symlink, and setting an environment variable pointing to the config.

When it loads it traps all the loading errors from missing modules and maps them to installers that download and build all the third party modules needed by the config.

It can mostly load on the first try, and usually works well after a reboot.

## Other goodies

It also contains a PEG parser compiler Macro which is like a Elisp Mobius strip of meta-programming. It's called parser.el
