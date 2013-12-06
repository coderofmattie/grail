;;---------------------------------------------------------------------
;; search-paths
;;----------------------------------------------------------------------
(require 'search-trees)

(search-trees-add-for-host "rouge" "cperl-mode"
  '("classic"   "/home/codermattie/work/inventory-codebase")
  '("core"      "/home/codermattie/work/inventory-codebase")
  '("inventory" "/home/codermattie/work/inventory-codebase")
  '("system"
     "/usr/local/lib/perl/5.14.2"
     "/usr/local/share/perl/5.14.2"
     "/usr/lib/perl5"
     "/usr/share/perl5"
     "/usr/lib/perl/5.14"
     "/usr/share/perl/5.14"
     "/usr/local/lib/site_perl")
  '("cobalt-web"
     "/web/lib/"
     "/web/cobalt/"
     "/web/auto/lib/"
     "/web/auto/import/lib/"
     "/web/auto/export/inventory/"))

(search-trees-add-for-user "codermattie" "emacs-lisp-mode"
  '("emacs" "/home/codermattie/config/emacs/local"))
