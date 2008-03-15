(require 'closure)

(closure-define test-closure
  (red "hot")
  (blue "cold"))

(pp-closure (closure-create test-closure))
(setq foo-closure (closure-create test-closure))

(save-lexical-closure foo-closure
  (message "red is %s" red)
  (message "blue is %s" blue))


  (message
    (if (string-equal red "hot")
      "definitely hot!"
      "bad boom!"))

  t)
