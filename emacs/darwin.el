;;----------------------------------------------------------------------
;; darwin.el
;; emacs does not get environment variables set from .profile and like
;; because it is launched by finder. Correct key environment variables.
;;----------------------------------------------------------------------

;; for best font support on a laptop LCD, the font hinting setting of "medium"
;; in the apple control panel works best, along with disabling anti-aliasing.
(setq mac-allow-anti-aliasing nil)

(setq platform-font-family '("DejaVu LGC Sans Mono" "DejaVu Sans Mono" "Trebuchet MS"))

(mask-grail-profiles "spell")

(defun osx-merge-etc-paths-file ()
  (append
    (split-string (with-temp-buffer
                    (insert-file-contents-literally "/etc/paths")

                    (buffer-substring-no-properties
                      (progn
                        (beginning-of-buffer)
                        (point))
                      (progn
                        (end-of-buffer)
                        (point)) )) )
    exec-path))

;; unfuck osx which has no sane way to handle environment variables
;; or paths. only launchd.conf in /etc which sets variables globally
;; is honored.
;;
;; paths which works better is not honored except in the shell.

(setq exec-path (osx-merge-etc-paths-file))

(require 'erc)
(require 'socks)

(add-hook 'erc-mode-hook
  (lambda ()
    (setq socks-noproxy '("localhost"))
    (setq erc-server-connect-function 'socks-open-network-stream)

    (setq socks-server (list "tor proxy" "localhost" 9050 5)) )
  t)



















