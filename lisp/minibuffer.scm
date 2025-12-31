;;; minibuffer.scm --- Minibuffer and completion functions

;;; Commentary:

;; Names with "--" are for functions and variables that are meant to be for
;; internal use only.

;;; Code:

(define minibuffer-mode-map (make-sparse-keymap))


(define-key minibuffer-mode-map "RET" minibuffer-complete-and-exit)

(define-derived-mode minibuffer-mode nil "Minibuffer"
  "Major mode used for active minibuffers.

For customizing this mode, it is better to use
`minibuffer-setup-hook' and `minibuffer-exit-hook' rather than
the mode hook of this mode."
  (use-local-map minibuffer-mode-map))
