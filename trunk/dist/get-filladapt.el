(with-temp-buffer
  (url-insert-file-contents "http://www.wonderworks.com/download/filladapt.el")
  (write-file (concat grail-dist-elisp "filladapt.el")))
