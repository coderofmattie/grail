;;----------------------------------------------------------------------
;; dwim-complete
;;----------------------------------------------------------------------
(grail-load 'helm (grail-define-installer "helm"
                    "git"
                    "https://github.com/emacs-helm/helm.git"))

(require 'helm-config)
