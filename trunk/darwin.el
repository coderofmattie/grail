;;----------------------------------------------------------------------
;; darwin.el
;; emacs does not get environment variables set from .profile and like
;; because it is launched by finder. Correct key environment variables.
;;----------------------------------------------------------------------
(message "%s" "loading darwin system configuration")

;; for best font support on a laptop LCD, the font hinting setting of "medium"
;; in the apple control panel works best, along with disabling anti-aliasing.
(setq mac-allow-anti-aliasing nil)

;; (setq ns-use-qd-smoothing nil)

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

;; set the exec path for the mac-ports in case it was not set normally.
(setq exec-path (append         '( "/opt/local/bin"
                                   "/opt/local/sbin") exec-path))

(setenv "PATH"
  (string-join ":"

    (append
      (prefix-strings (getenv "HOME") ;; add User Local commands
        '( "/system/bin"
           "/projects/rc"
           "/projects/cherry" ))

      (prefix-strings "/opt/local/"
        '("bin" "sbin"))                ;; Mac Ports.

      (list (getenv "PATH")))          ;; existing PATH
    ))

;; (setenv "PERL5LIB"
;;   (string-join ":"
;;       (prefix-strings (getenv "HOME")

;;         '( "/projects/Fault"
;;            "/projects/rc"
;;            "/projects/cherry"
;;            "/projects/cmdline"
;;            "/projects/xstruct"
;;            "/projects/listy"
;;            "/system/cpan/lib/perl5/site_perl"
;;            "/system/cpan/lib/perl5/5.8.8"
;;            ))))

;; (setenv "TOOLKIT" "/usr/home/mattie/vcntl/reforged")
;; (setenv "WORK" "/usr/home/mattie/vcntl/commercial")


















