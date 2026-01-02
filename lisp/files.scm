;;; files.scm --- file input and output commands

;;; Commentary:

;; Defines Glemax's basic file- and directory-handling functions,
;; including file visiting, backup file generation and versioning,
;; link handling, load- and write-hook handling, and the like.

;;; Code:

(use-modules (ice-9 regex))

;; Auto-mode-alist: associates file patterns with major modes
;; Each entry is (PATTERN . MODE-FUNCTION-SYMBOL)
(define auto-mode-alist
  `((,"\\.c$" . c-mode)
    (,"\\.h$" . c-mode)
    (,"\\.scm$" . scheme-mode)
    (,"\\.py$" . python-mode)
    (,"\\.json$" . json-mode)
    (,"\\.rs$" . rust-mode)
    (,"\\.zig$" . zig-mode)))

(set-var-doc! auto-mode-alist
"Alist of file name patterns vs corresponding major mode functions.
Each element looks like (REGEXP . FUNCTION) or (REGEXP FUNCTION NON-NIL).
\(NON-NIL stands for anything that is not nil; the value does not matter.)
Visiting a file whose name matches REGEXP specifies FUNCTION as the
mode function to use.  FUNCTION will be called, unless it is nil.

If the element has the form (REGEXP FUNCTION NON-NIL), then after
calling FUNCTION (if it's not nil), we delete the suffix that matched
REGEXP and search the list again for another match.

The extensions whose FUNCTION is `archive-mode' should also
appear in `auto-coding-alist' with `no-conversion' coding system.

See also `interpreter-mode-alist', which detects executable script modes
based on the interpreters they specify to run,
and `magic-mode-alist', which determines modes based on file contents.")


(define (set-auto-mode)
  "Select major mode appropriate for current buffer.
This checks the file name against `auto-mode-alist'."
  (let ((filename (buffer-file-name)))
    (when filename
      ;; Try to match against auto-mode-alist
      (let loop ((alist auto-mode-alist))
        (cond
         ((null? alist)
          ;; No match found, use fundamental-mode
          (fundamental-mode))
         (else
          (let* ((entry (car alist))
                 (pattern (car entry))
                 (mode (cdr entry)))
            (if (string-match pattern filename)
                ;; Found a match - call the mode function
                (begin
                  ((module-ref (current-module) mode))
                  #t)
                ;; No match, try next entry
                (loop (cdr alist))))))))))

;; Hook to run set-auto-mode when finding files
(define find-file-hook '())

;; Add set-auto-mode to the find-file-hook
(setq find-file-hook (cons set-auto-mode find-file-hook))

