;;----------------------------------------------------------------------
;; darwin.el
;; emacs does not get environment variables set from .profile and like
;; because it is launched by finder. Correct key environment variables.
;;----------------------------------------------------------------------
(message "%s" "darwin system detected")

(defun export-ssh-agent ()
  (load-file (concat (getenv "HOME") "/.emacs-ssh-agent")))

(setenv "PATH"
  (concat
    (getenv "PATH")

    (string-join ":"
      (append '()
        '("/sw/bin"
           "/sw/sbin")

        (string-prefix (getenv "HOME")
          '( "/projects/rc"
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

(export-ssh-agent)


















