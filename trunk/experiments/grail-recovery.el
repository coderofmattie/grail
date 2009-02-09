(grail-in-load-path-p "mic-parens") ;; should fail
(grail-in-load-path-p "mic-paren")  ;; should succeed.

(message "test the grail diagnostics %s"
  (format-signal-trap (diagnostic-load-elisp (error "%s" "bad things happened"))))

(defmacro grail-initialize-with-recovery ( style package installer &rest initializer )
  "grail-load-dep-with-recovery STYLE PKG-SYMBOL PKG-INSTALL INITIALIZER

   attempt to load PKG-SYMBOL via require. If there are errors generate a repair function
   that: installs the package and re-attempts loading and initialization.
  "
  (let
    ((pkg-name        (symbol-name package))
     (report-error    (lambda ( failure diagnostic repair-function )
                        (format
                          "grail: style %s is degraded from dependency %s failures %s\n; uncomment the function below to attempt repair\
\n; (%s) " style pkg-name failure repair-function)))
     (diagnostic   nil))

    (when (catch 'abort ;; non-local exit for early termination from errors.

      ;; try loading the package catching any diagnostic errors from signals
      (when (setq diagnostic (diagnostic-load-elisp (require ',package)))

        ;; create a recovery function.
        (let
          ((repair-fn-name (concat "repair-dependency-" pkg-name)))

          ;; bind one of two recovery variations: a debugging setup if
          ;; it is found, or an installer if the package is not found
          ;; in load path.

          (fset (intern repair-fn-name)
            (if (grail-in-load-path-p pkg-name)
              (progn
                (funcall report-error "package loading" (format-signal-trap diagnostic) repair-fn-name)

                (lambda ()
                  (interactive)
                  (grail-repair-by-debugging ',pkg-symbol) ))

              (progn
                (funcall report-error "package installation"
                  "cannot find package in load-path" repair-fn-name)

                (lambda ()

                  )) )))

        (throw 'abort t))

      (unless (robust-load-elisp ,initializer)
          (grail-dup-error-to-scratch "initialization failed")
          (throw 'abort nil))

            (grail-dup-error-to-scratch
              (format "grail: style %s is degraded from failure to load %s\n; uncomment the function below to attempt repair\
\n; (%s) " style pkg-name repair-fn-name)) )

          (throw 'abort nil) )

        t))))
