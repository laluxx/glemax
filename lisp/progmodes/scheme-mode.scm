;;; scheme-mode.scm --- Scheme editing mode

;;; Commentary:

;; The major mode for editing Scheme-type Lisp code.

;;; Code:

(define scheme-treesit-highlight-query
  "
  ; Comments
  (comment) @comment

  ; Numbers  
  (number) @number

  ; Strings
  (string) @string

  ; Booleans
  (boolean) @boolean

  ; Symbols/identifiers
  (symbol) @variable

  ; Special forms - these may need adjustment based on your grammar
  ; (list . (symbol) @keyword)
")

(define scheme-mode-map (make-sparse-keymap))

(define (scheme-test-1)
  (message "Scheme mode binding 1"))

(define (scheme-test-2)
  (message "Scheme mode binding 2"))

(define-key scheme-mode-map "C-j" scheme-test-1)
(define-key scheme-mode-map "C-k" scheme-test-2)

(define (scheme-mode-setup-treesitter)
  "Initialize tree-sitter for Scheme mode."
  (when (treesit-language-available? 'scheme)
    ;; Create parser for current buffer
    (if (treesit-parser-create 'scheme)
        (begin
          ;; Set the highlight query
          (if (treesit-set-highlight-query! scheme-treesit-highlight-query)
              (begin
                ;; Apply highlights
                (treesit-apply-highlights)
                )
              ))
        )))

(define-derived-mode scheme-mode #f "Scheme"
  "Major mode for editing Scheme code."
  (use-local-map scheme-mode-map)
  (setq-local 'tab-width 2)
  ;; Initialize tree-sitter if available
  (scheme-mode-setup-treesitter))
