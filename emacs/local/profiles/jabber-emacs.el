;;----------------------------------------------------------------------
;; jabber-emacs
;;----------------------------------------------------------------------
(grail-load 'jabber (grail-define-installer "jabber"
                     "git"
                     "git://git.code.sf.net/p/emacs-jabber/git"))

(setq jabber-auto-reconnect t)

(defun codermattie-im ()
  "codermattie-im

   connect to jabber account
  "
  (interactive)

  (jabber-connect
    "codermattie"
    "xmppnet.de"
    "emacs") )

(require 'buffer-ring)

(defun jabber-emacs-in-ring ()
  (configure-for-buffer-ring "jabber") )

(add-hook 'jabber-chat-mode-hook 'jabber-emacs-in-ring t)
(add-hook 'jabber-roster-mode-hook 'jabber-emacs-in-ring t)



