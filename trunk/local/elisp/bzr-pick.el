;;;----------------------------------------------------------------------
;; bzr-pick.el
;; written by Mike Mattie
;; copyright: Mike Mattie 2009
;;;----------------------------------------------------------------------

(defconst bzr-pick-version "0.1.0")

(require 'cm-path)
(require 'rc-nagivate)

(def-sparse-map bzr-pick-overlay-map
  "bzr pick merge overlay map"
  ("d" 'bzr-pick-merge-diff)
  ("m" 'bzr-pick-merge-do-merge)
  ("n" 'bzr-pick-next-commit)
  ("p" 'bzr-pick-prev-commit))

(defface bzr-pick-revision-face
  '((t :weight bold))
  "The bzr-pick face for the commit revision identifier.")

(defface bzr-pick-selected-face
  '((t :foreground "khaki3"))
  "Face for Info node names.")

(defface bzr-pick-unselected-face
  '((t :foreground "grey70"))
  "The bzr-pick face for a not selected face who's state has not changed.")

(defface bzr-pick-error-face
  '((t :foreground "firebrick1"))
  "The bzr-pick face for a commit for a conflicted merge.")

(defface bzr-pick-merged-face
  '((t :foreground "sea green"))
  "The bzr-pick face for a commit already merged.")

(defface bzr-pick-conflicted-face
  '((t :foreground "orange1"))
  "The bzr-pick face for a commit that was conflicted.")

(defvar bzr-pick-before-merge-hook nil
  "Hooks run in the merge buffer after it has been setup but before the interface is presented.")

;;----------------------------------------------------------------------
;; commit ADT
;;----------------------------------------------------------------------

(defun bzr-pick-commit-struct ( rev-id start end )
  (cons (cons rev-id nil) (cons start end)))

(defun bzr-pick-commit-revid ( struct )
  (car (car struct)))

(defun bzr-pick-commit-overlay ( struct )
  (cdr (car struct)))

(defun bzr-pick-commit-start ( struct )
  (car (cdr struct)))

(defun bzr-pick-commit-end ( struct )
  (cdr (cdr struct)))

(defun bzr-pick-commit-set-overlay ( struct overlay )
  (setcdr (car struct) overlay)
  struct)

(defun bzr-pick-commit-status ( struct )
  (overlay-get (bzr-pick-commit-overlay struct) 'status))

(defun bzr-pick-commit-array ( commit-list )
  "bzr-pick-commit-array LIST

   Given a LIST of commits create and return a vector of the
   commits. The size of the vector must be known at creation time
   so this converts from a list of a given size to a static
   vector.
  "
  (if commit-list
    (let
      ((commit-array  (make-vector (length commit-list) nil))
        (i 0))

      (mapc
        (lambda ( commit )
          (aset commit-array i commit)
          (setq i (+ i 1)))
        commit-list)

      commit-array)
    nil))

;;----------------------------------------------------------------------
;; buffer setup
;;----------------------------------------------------------------------

(defun bzr-pick-search-next-commit ()
  (interactive)
  ;; search for the next commit. If it finds nothing don't
  ;; signal an error, just return nil.
  (search-forward-regexp "^---+\n+" nil t))

(defun bzr-pick-extract-revid ()
  (interactive)

  ;; we could be really strict on what is accepted as a revision
  ;; number but that would be very brittle. revision numbers don't
  ;; always have to be whole numbers.

  (when (looking-at "\\(revno:\\)\\ *\\(.+\\)$")
    (set-text-properties (match-beginning 1) (match-end 1) '(face bzr-pick-revision-face))
    (match-string 2) ))

(defun bzr-pick-scan-commits ()
  "bzr-pick-scan-commits

   Scan through the current buffer from the point to the end
   building commit structures out of a regex traversal. A list of
   commit structs is returned.
  "
  (let
    ((pos (bzr-pick-search-next-commit))
     (commit-list nil))

    (catch 'done
      (unless pos (throw 'done nil))

      (while pos
        (let
          ((commit-start pos)
           (rev-id (bzr-pick-extract-revid))
           (commit-end nil))

          ;; when the revision id can't be extracted the scan can't be trusted so abort.
          (unless rev-id
            (signal 'error (format "bzr-pick-scan-commits could not extract a revision id @ %d" pos)))

          ;; find the next commit.
          (if (setq commit-end (bzr-pick-search-next-commit))
            ;; set the starting position to the beggining of the commit for the
            ;; next iteration.
            (progn
              (setq commit-list (cons (bzr-pick-commit-struct rev-id commit-start commit-end) commit-list))
              (setq pos commit-end))

            (when (> (buffer-end 1) commit-start)
              ;; it's reversed but that is good since the missing output like log is
              ;; last to first.
              (throw 'done (cons (bzr-pick-commit-struct rev-id commit-start (buffer-end 1))
                                      commit-list))) )))
      nil)))

(defun bzr-pick-make-overlay ( struct )
  (bzr-pick-commit-set-overlay  struct (make-overlay
                                         (bzr-pick-commit-start struct)
                                         (bzr-pick-commit-end struct))))

(defun bzr-pick-face-for-status ( status selected )
  (unless (symbolp status)
    (signal 'error "multiple states in overlays not yet supported"))

  (cond
    ((equal 'merged status)      'bzr-pick-merged-face)
    ((equal 'error status)       'bzr-pick-error-face)
    ((equal 'conflicted status)  'bzr-pick-conflicted-face)
    ((not status)
      (if selected
        'bzr-pick-selected-face
        'bzr-pick-revision-face
        'bzr-pick-unselected-face)) ))

(defun bzr-pick-set-commit-current ( current )
  "bzr-pick-set-commit-current

   Set the commit overlay as current by setting the previous
   selected overlay if any back to it's status computed face, and
   set the new current overlay's face by both status and
   selected.
  "
  (when selected-commit
    (let
      ((overlay (bzr-pick-commit-overlay (aref commit-array selected-commit))))
      (overlay-put overlay 'face (bzr-pick-face-for-status (overlay-get overlay 'status) nil)) ))

  (setq selected-commit current)

  (let*
    ((commit (aref commit-array current))
     (overlay (bzr-pick-commit-overlay commit)))

    (overlay-put overlay 'face (bzr-pick-face-for-status (overlay-get overlay 'status) t))
    (goto-char (bzr-pick-commit-start commit)) ))

(defun bzr-pick-setup-merge-buffer ()
  "bzr-pick-setup-merge-buffer

   With the current buffer scan it and setup
  "
  (remove-overlays)
  (goto-char 0)

  (set (make-local-variable 'commit-array) (bzr-pick-commit-array (bzr-pick-scan-commits)))
  (if commit-array
    (progn
      (let
        ((i 0)
          (commit-length (length commit-array))
          (overlay nil))

        (while (< i commit-length)
          (setq overlay
            (bzr-pick-commit-overlay
              (aset commit-array i (bzr-pick-make-overlay (aref commit-array i))) ))

          ;; default settings for the overlay

          (overlay-put overlay 'keymap bzr-pick-overlay-map)
          (overlay-put overlay 'face   'bzr-pick-unselected-face)

          ;; status is the state of the overlay,
          ;; a single element or list of: merged,conflicted,error, nil,
          (overlay-put overlay 'status nil)
          (overlay-put overlay 'commit-index i)

          (setq i (+ i 1)) ))

      (set (make-local-variable 'selected-commit) nil)

      (bzr-pick-set-commit-current 0)
      t)
    nil))

(defun trial ()
  (interactive)

  (switch-to-buffer (get-buffer "bzr-merge"))
  (bzr-pick-setup-merge-buffer))

;;----------------------------------------------------------------------
;; interface commands
;;----------------------------------------------------------------------

(defun bzr-traverse ( wrap-bound wrap-start next in-bounds )
  (let
    ((previous selected-commit)
     (i nil)
     (wrapped-around nil))

    (catch 'done
      ;; handle the selected-commit unitialized case.
      (unless previous
        (setq i 0)
        (throw 'done t))

      ;; initialize i to current-commit. It is called previous
      ;; so we can detect motion.
      (setq i previous)

      ;; allow it to wrap around _once_
      (while (not wrapped-around)
        (when (funcall wrap-bound i)
          (setq wrapped-around t)
          (setq i wrap-start))

        ;; move it before the traversal iteration so that we don't end
        ;; up moving beyond the end of the array after the end of array
        ;; test.
        (setq i (funcall next i))

        ;; the traversal iteration.
        (while (funcall in-bounds i)
          (let
            ((commit-status (bzr-pick-commit-status (aref commit-array i))))

            ;; we are looking for nil, or error statuses which indicate that
            ;; there is something to do.
            (cond
              ((not commit-status)          (throw 'done t))
              ((equal 'error commit-status) (throw 'done t))) )

          (setq i (funcall next i)) )))

    (if (equal previous i)
      (progn
        (message "No unmerged or failed commits remaining")
        nil)
      (progn
        (bzr-pick-set-commit-current i)
        t)) ))

(defun bzr-traverse-forward-wrap-bound ( i )
  (when (>= i (length commit-array)) t))

(defun bzr-traverse-forward-in-bounds ( i )
  (when (< i (length commit-array)) t))

(defun bzr-traverse-forward-next ( i ) (+ i 1))

(defconst bzr-traverse-forward-wrap -1)

(defun bzr-pick-next-commit ()
  (interactive)
  (bzr-traverse
    'bzr-traverse-forward-wrap-bound
    bzr-traverse-forward-wrap
    'bzr-traverse-forward-next
    'bzr-traverse-forward-in-bounds))

(defun bzr-traverse-backward-wrap-bound ( i ) (when (< i 0) t))

(defun bzr-traverse-backward-in-bounds ( i ) (when (>= i 0) t))

(defun bzr-traverse-backward-next ( i ) (- i 1))

(defun bzr-pick-prev-commit ()
  (interactive)
  (bzr-traverse
    'bzr-traverse-backward-wrap-bound
    (length commit-array)
    'bzr-traverse-backward-next
    'bzr-traverse-backward-in-bounds))

(defun bzr-pick-merge-sync-commit ()
  (let
    ((commit-status (vc-bzr-command "commit" merge-output t
                      nil "-m" (format "pick merge revid %s" (bzr-pick-commit-revid commit)))))

    (cond
      ((equal commit-status 0)
        (progn
          (setq advance t)
          'merged))
      (t 'error)) ))

(defun bzr-pick-merge-commit ( commit )
  (let*
    ((default-directory left-hand)
     (merge-output (get-buffer-create "*bzr-merge*"))
     (bzr-status nil)
     (advance nil))

    ;; attempt the merge. note that merges are forced so
    ;; we don't have to commit after every merge.
    (setq bzr-status
      (vc-bzr-command "merge" merge-output  t
        right-hand
        "--force" "-c" (bzr-pick-commit-revid commit)) )

    ;; set the status based on exit value. The exit values are not well
    ;; documented apparently.

    (overlay-put (bzr-pick-commit-overlay commit) 'status
      (cond
        ((equal 0 bzr-status)
          (bzr-pick-merge-sync-commit))
        ((equal 3 bzr-status)
          (progn
            (message "Merge Error!")
            (pop-to-buffer merge-output)
            'error))
        (t (progn
             (message "Merge Conflict!")
             'conflicted))))

    ;; goto the next commit
    (when advance (bzr-pick-next-commit)) ))

(defun bzr-pick-merge-do-merge ()
  (interactive)
  (bzr-pick-merge-commit (aref commit-array selected-commit)))

(defun bzr-pick-diff-commit ( branch revid )
  (let*
    ((buffer-name (format "*bzr-diff[%s]*" revid))
     (diff-buffer (get-buffer-create buffer-name))
     (default-directory branch)
     (status (vc-bzr-command "diff" diff-buffer t nil "-c" revid)))

    ;; add diff buffers to a list so we can later kill'em all
    (setq diff-buffers (add-to-list 'diff-buffers buffer-name))

    ;; The exit status is atypical.
    ;; 1 = changes
    ;; 2 = unrepresentable changes
    ;; 3 = error
    ;; 0 = no changes.

    (with-current-buffer diff-buffer
      (if (equal 1 status)
        (progn
          (diff-mode)
          diff-buffer)

        (progn
          (set-buffer-modified-p nil)
          (kill-buffer diff-buffer)
          nil))) ))

(defun bzr-pick-merge-diff ()
  (interactive)
  (let
    ((diff-buffer (bzr-pick-diff-commit right-hand
                    (bzr-pick-commit-revid (aref commit-array selected-commit)))))

    (if diff-buffer
      (progn
        (pop-to-buffer diff-buffer)
        t)
      (progn
        (message "could not retrieve diff")
        nil)) ))

(defun bzr-pick-cleanup-diff-buffers ()
  (mapc
    (lambda (buffer-name)
      (let
        ((buffer-object (get-buffer buffer-name)))

        (when (bufferp buffer-object)
          (with-current-buffer buffer-object (set-buffer-modified-p nil))
          (kill-buffer buffer-object)) ))

    diff-buffers)
  (setq diff-buffers nil))

(defun bzr-resolve-conflict ( branch &rest files )
  (mapc
    (lambda ( file )
      (let
        ((base  (file-path-if-readable (concat branch "/" file ".BASE")))
         (trunk (concat branch "/" file ".THIS"))
         (tip   (concat branch "/" file ".OTHER")))

        (if base
          (ediff-merge-with-ancestor trunk tip base)
          (ediff-merge-files trunk tip)) ))
        files))

(defun bzr-pick-merge ()
  (interactive)
  (let*
    ((right-hand-branch  (bzr-prompt-for-branch "Merge from Branch ? "))
     (left-hand-branch   (bzr-prompt-for-branch "Merge to Branch ? "
                    (bzr-find-repository-top right-hand-branch))))

    (catch 'abort
      (unless (vc-bzr-root right-hand-branch)
        (message "the branch to merge from (left hand) is not a bzr branch")
        (throw 'abort nil))

      (unless (vc-bzr-root left-hand-branch)
        (message "the branch to merge to (right hand) is not a bzr branch")
        (throw 'abort nil))

      (message "Merging from %s to %s" right-hand-branch left-hand-branch)

      (let
        ((default-directory right-hand-branch)
         (merge-select-buffer (get-buffer-create "*bzr-merge*")) )

        (with-current-buffer merge-select-buffer
          (set (make-local-variable 'left-hand)  left-hand-branch)
          (set (make-local-variable 'right-hand) right-hand-branch)

          (set (make-local-variable 'diff-buffers) nil)

          (unless (buffer-empty-p)
            (erase-buffer))

          (vc-bzr-command "missing" merge-select-buffer t left-hand "-v")

          (unless (bzr-pick-setup-merge-buffer)
            (message "No unmerged commits could be found !")
            (throw 'abort nil))

          (run-hooks 'bzr-pick-before-merge-hook)
          ;; add a buffer local hook to clean up the diff buffers
          (add-hook 'kill-buffer-hook 'bzr-pick-cleanup-diff-buffers t t))

        (pop-to-buffer merge-select-buffer t))
      t)))

(provide 'bzr-pick)
