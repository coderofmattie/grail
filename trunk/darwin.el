;;----------------------------------------------------------------------
;; darwin.el
;; emacs does not get environment variables set from .profile and like
;; because it is launched by finder. Correct key environment variables.
;;----------------------------------------------------------------------
(message "%s" "loading darwin system configuration")

(setq mac-option-modifier 'meta)  ;; oh happy day !!!
                                  ;; no more hellish binding of meta
                                  ;; on the mac key. can use CarbonEmacs again.

(setenv "PATH"
  ;; adjust PATH to locate commands
  (concat
    (getenv "PATH")

    ;; add the third party package manager directories

    (string-join ":"
      (append '()

        ;; macports
        '( "/opt/local/bin"
           "/opt/local/sbin")

        ;; add User Local commands

        (string-prefix (getenv "HOME")
          '( "/system/bin"
             "/projects/rc"
             "/projects/cherry" ))
        ))))

(setenv "PERL5LIB"
  (string-join ":"
      (string-prefix (getenv "HOME")
        '( "/projects/rc"
           "/projects/cherry"
           "/projects/cascade"
           "/projects/cmdline"
           "/projects/xstruct"
           "/projects/listy"))))


















