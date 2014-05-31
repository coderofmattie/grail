;;----------------------------------------------------------------------
;; interface.el
;;----------------------------------------------------------------------

;; get rid of that horrible dinging
(setq visible-bell t)

;; disable things I don't use from eating screen-space

(tool-bar-mode -1)                            ;; cannot be set with setq
(scroll-bar-mode -1)                          ;; disable the scrollbar
(menu-bar-mode -1)                            ;; disable the menu bar as well

;; mode-line customization

(display-time)                            ;; display the time on the modeline

(column-number-mode 1)                    ;; handy guides when following
(line-number-mode 1)                      ;; errors

(toggle-uniquify-buffer-names)                ;; more intelligent unique buffer names, will automatically
                                              ;; simplify buffer names when collisions are reduced.

(require 'mattie-modeline)                 ;; my own modeline setup
(setup-mattie-modeline)

(setq initial-scratch-message nil)            ;; nix the scratch message, and the splash screen.
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)

(setq global-font-lock-mode t)                ;; turn on font lock mode globally

(transient-mark-mode -1)                        ;; not a big fan of transient mark mode.

(setq-default set-mark-command-repeat-pop t)  ;; C-u C-<spc> pops the mark, with this on
                                              ;; simply repeating C-<spc> continues backwards through
                                              ;; the ring. makes it easier to rewind back through a
                                              ;; series of marks.

(fset 'yes-or-no-p 'y-or-n-p)                 ;; y/n instead of yes/no

(put 'erase-buffer 'disabled nil)             ;; enable erase-buffer, no hand-holding.
