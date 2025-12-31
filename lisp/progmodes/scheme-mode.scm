;;; scheme-mode.scm --- Scheme editing mode

;;; Commentary:

;; The major mode for editing Scheme-type Lisp code.

;;; Code:

(define scheme-treesit-highlight-query
  "
  ; Comments
  [(comment) (block_comment) (directive)] @comment
  
  ; Strings
  (string) @string
  
  ; Numbers
  (number) @number
  
  ; Keywords - control structures and special forms
  (list
    .
    (symbol) @keyword
    (#match? @keyword \"^(use-package|defvar-local|defvar|defun|progn|setq-local|setq|begin|call-with-current-continuation|call/cc|call-with-input-file|call-with-output-file|call-with-port|case|cond|do|else|for-each|if|lambda|λ|let|let\\\\*|let-syntax|letrec|letrec-syntax|letrec\\\\*|export|import|let-values|let\\\\*-values|and|or|delay|force|map|syntax|syntax-rules|when|while|unless|include|include-ci|cond-expand|delay-force|parameterize|guard|case-lambda|syntax-error|only|except|prefix|rename|define-values|define-record-type|define-library|include-library-declarations|receive|quote|quasiquote|unquote|unquote-splicing)$\"))
  
  ; Define forms - highlight the keyword
  (list
    .
    (symbol) @keyword
    (#match? @keyword \"^(define|define-syntax|define-macro|define-values|define-record-type|define-library)$\"))
  
  ; Function names in define forms
  (list
    .
    (symbol) @keyword
    (#eq? @keyword \"define\")
    .
    (list
      .
      (symbol) @function))
  
  (list
    .
    (symbol) @keyword
    (#eq? @keyword \"define\")
    .
    (symbol) @function)
  
  ; Function names in defun forms (Emacs Lisp style)
  (list
    .
    (symbol) @keyword
    (#eq? @keyword \"defun\")
    .
    (symbol) @function)

  ; Function names in define-syntax forms
  (list
    .
    (symbol) @keyword
    (#eq? @keyword \"define-syntax\")
    .
    (symbol) @function)
  
  ; Named let
  (list
    .
    (symbol) @keyword
    (#eq? @keyword \"let\")
    .
    (symbol) @function)
  
  ; Library declarations
  (list
    .
    (symbol) @keyword
    (#eq? @keyword \"library\")
    .
    (list
      .
      (symbol)? @type))
  

   ; R6RS/R7RS keywords with #: prefix
   (keyword) @constant.builtin
   
   ; SRFI-88 keywords with : prefix
   ((symbol) @constant.builtin
    (#match? @constant.builtin \"^:[-a-zA-Z0-9_!$%&*+./:<=>?@^_~]+$\"))


  ; Type annotations <type>
  ((symbol) @type
   (#match? @type \"^<[a-zA-Z][-a-zA-Z0-9_]*>$\"))

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

(define-derived-mode scheme-mode prog-mode "Scheme"
  "Major mode for editing Scheme code."
  (use-local-map scheme-mode-map)
  (setq-local tab-width 2)
  ;; Initialize tree-sitter if available
  (scheme-mode-setup-treesitter))
