;;; monad-mode.scm --- major mode for editing Monad

;;; Commentary:

;; major mode for editing Monad

;;; Code:

(define monad-treesit-highlight-query
  "
  ; Comments
  (line_comment) @comment
  (block_comment) @comment

  ; Keywords
  \"define\" @keyword
  \"lambda\" @keyword
  \"match\" @keyword
  \"layout\" @keyword
  \"type\" @keyword
  \"data\" @keyword
  \"deriving\" @keyword
  \"let\" @keyword
  \"letrec\" @keyword
  \"let*\" @keyword
  \"if\" @keyword
  \"cond\" @keyword
  \"case\" @keyword
  \"else\" @keyword
  \"begin\" @keyword
  \"when\" @keyword
  \"unless\" @keyword
  \"error\" @keyword
  \"instance\" @keyword
  \"asm\" @keyword
  \"module\" @keyword
  \"import\" @keyword
  \"qualified\" @keyword
  \"hiding\" @keyword
  \"tests\" @keyword
  \"test\" @keyword
  \"include\" @keyword
  \"until\" @keyword
  \"for\" @keyword
  \"while\" @keyword
  \"class\" @keyword
  \"where\" @keyword
  \"set!\" @keyword
  \"set\" @keyword

  ; Operators
  \"::\" @operator
  \"->\" @operator
  \"'\" @operator
  \"`\" @operator
  \",\" @operator
  \",@\" @operator

  ; Literals
  (string) @string
  (character) @string
  (escape_sequence) @string.escape
  (number) @number
  (hex_number) @number
  (float) @number
  (boolean) @constant.builtin

  ; C include path
  (include_expression (c_include) @string)

  ; Types
  (type_name) @type
  (module_name) @type

  ; Function definitions
  (function_definition
    header: (function_header
      name: (identifier) @function))

  ; Variable definitions
  (variable_definition
    name: (typed_annotation
      name: (identifier) @variable))
  (variable_definition
    name: (identifier) @variable)

  ; Parameters
  (typed_param
    name: (identifier) @variable.parameter)

  ; Wildcard
  (wildcard) @variable.builtin

  ; Feature flags
  (feature_flag) @keyword
  (feature_flag (identifier) @function.builtin)

  ; Assembly
  (asm_label (identifier) @label)
  (asm_instruction mnemonic: (identifier) @function.builtin)
  (asm_register) @variable.builtin
  (asm_immediate) @number

  ; Infix backtick
  (infix_expression (identifier) @variable.parameter)

  ; Function calls — first position
  (call_expression
    . (identifier) @function.call)
  ")

(define monad-mode-map (make-sparse-keymap))

(define (monad-mode-setup-treesitter)
  "Initialize tree-sitter for Monad mode."
  (when (treesit-language-available? 'monad)
    ;; Create parser for current buffer
    (if (treesit-parser-create 'monad)
        (begin
          ;; Set the highlight query
          (if (treesit-set-highlight-query! monad-treesit-highlight-query)
              (begin
                ;; Apply highlights
                (treesit-apply-highlights)
                )
              ))
        )))

(define-derived-mode monad-mode prog-mode "Monad"
  "Major mode for editing Monad code."
  (use-local-map monad-mode-map)
  (setq-local tab-width 2)
  ;; Initialize tree-sitter if available
  (monad-mode-setup-treesitter))

