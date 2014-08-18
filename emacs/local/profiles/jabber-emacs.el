;;----------------------------------------------------------------------
;; jabber-emacs
;;----------------------------------------------------------------------
(require 'buffer-ring)

(grail-load 'jabber "git" "git://git.code.sf.net/p/emacs-jabber/git")

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

(defun jabber-emacs-in-ring ()
  (buffer-ring/add "jabber")
  (buffer-ring/local-keybindings))

(add-hook 'jabber-chat-mode-hook 'jabber-emacs-in-ring t)
(add-hook 'jabber-roster-mode-hook 'jabber-emacs-in-ring t)

(provide 'profile/jabber)


