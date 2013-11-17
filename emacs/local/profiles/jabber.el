;;----------------------------------------------------------------------
;; jabber
;;----------------------------------------------------------------------

(grail-load 'jabber     (grail-define-installer "jabber"
                         "pkg"
                         'jabber))


(defun connect-to-im ()
  (interactive)

  (jabber-connect "codermattie@xmppnet.de" "xmppnet.de"))

(setq jabber-account-list
  '( ("codermattie@xmppnet.de"
      (:connection-type . ssl)
      (:network-server . "xmppnet.de")) ))
