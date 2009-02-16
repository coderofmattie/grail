(with-temp-buffer
  (url-insert-file-contents "http://www.neilvandyke.org/quack/quack.el")
  (write-file (concat grail-dist-elisp "quack.el")))
