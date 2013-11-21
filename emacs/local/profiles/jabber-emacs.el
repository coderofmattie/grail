;;----------------------------------------------------------------------
;; jabber-emacs
;;----------------------------------------------------------------------
(grail-load 'jabber (grail-define-installer "jabber"
                     "pkg"
                     'jabber))


(defun connect-to-im ()
  (interactive)

  (jabber-connect "codermattie@xmppnet.de" "xmppnet.de" nil nil nil nil 5222 'ssl))

(setq jabber-account-list
  '( ("codermattie@xmppnet.de"
      (:connection-type . starttls)
      (:network-server . "xmppnet.de")
      (:port . 5222)) ))
