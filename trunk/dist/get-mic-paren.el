(with-temp-buffer
  (url-insert-file-contents "http://www.emacswiki.org/cgi-bin/emacs/download/mic-paren.el")
  (write-file (concat grail-dist-elisp "mic-paren.el")))
