;;----------------------------------------------------------------------
;; darwin.el
;; emacs does not get environment variables set from .profile and like
;; because it is launched by finder. Correct key environment variables.
;;----------------------------------------------------------------------
(message "%s" "loading darwin system configuration")

(setq mac-option-modifier 'meta)  ;; oh happy day !!!
                                  ;; no more hellish binding of meta
                                  ;; on the mac key. can use CarbonEmacs again.

(custom-set-variables
  '(diff-command "gdiff")         ;; when invoking the shell on darwin aliases defined
                                  ;; for a interactive shell cause the diff command to
                                  ;; fail. Setting the diff command to gdiff provided
                                  ;; by macports solves the problem without interfering
                                  ;; with the alias'd diff command.

  '(ediff-diff-program "gdiff")
  '(ediff-diff3-program "gdiff3")
  '(ediff-diff3-options "--diff-program=gdiff")
  )

(setenv "PATH"
  ;; adjust PATH to locate commands
  (concat

    (string-join ":"
      (append '()

        (string-prefix-list (getenv "HOME") ;; add User Local commands
          '( "/system/bin"
             "/projects/rc"
             "/projects/cherry" ))

        '( "/opt/local/bin"
           "/opt/local/sbin") ;; macports

        )) ;; add the third party package manager directories

    ":" (getenv "PATH")
    ))

(setenv "PERL5LIB"
  (string-join ":"
      (string-prefix (getenv "HOME")

        '( "/projects/Fault"
           "/projects/rc"
           "/projects/cherry"
           "/projects/cmdline"
           "/projects/xstruct"
           "/projects/listy"))))

(setenv "TOOLKIT" "/usr/home/mattie/vcntl/reforged")
(setenv "WORK" "/usr/home/mattie/vcntl/commercial")


















