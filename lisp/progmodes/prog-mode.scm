;;; json-mode.scm --- A major-mode for editing json

;;; Commentary:

;; This major mode is mostly intended as a parent of other programming
;; modes.  All major modes for programming languages should derive from this
;; mode so that users can put generic customization on prog-mode-hook.


;;; Code:

(define-derived-mode fundamental-mode #f "Fundamental"
  "Major mode not specialized for anything in particular.
Other major modes are defined by comparison with this one."
  (kill-all-local-variables)
  ;; TODO (run-mode-hooks)
  )

(define prog-mode-map (make-sparse-keymap))


(define-derived-mode prog-mode fundamental-mode "Prog"
  "Major mode for editing programming language source code."
  (use-local-map prog-mode-map)
  )
