;;; c-mode.scm --- major mode for editing C

;;; Commentary:

;; major mode for editing C

;;; Code:

(define c-treesit-highlight-query
  "
  ; Comments
  (comment) @comment

  ; Preprocessor
  (preproc_directive) @keyword
  \"#define\" @keyword
  \"#elif\" @keyword  
  \"#else\" @keyword
  \"#endif\" @keyword
  \"#if\" @keyword
  \"#ifdef\" @keyword
  \"#ifndef\" @keyword
  \"#include\" @keyword

  ; Keywords
  \"break\" @keyword
  \"case\" @keyword
  \"const\" @keyword
  \"continue\" @keyword
  \"default\" @keyword
  \"do\" @keyword
  \"else\" @keyword
  \"enum\" @keyword
  \"extern\" @keyword
  \"for\" @keyword
  \"if\" @keyword
  \"inline\" @keyword
  \"return\" @keyword
  \"sizeof\" @keyword
  \"static\" @keyword
  \"struct\" @keyword
  \"switch\" @keyword
  \"typedef\" @keyword
  \"union\" @keyword
  \"volatile\" @keyword
  \"while\" @keyword

  ; Operators
  \"--\" @operator
  \"-\" @operator
  \"-=\" @operator
  \"->\" @operator
  \"=\" @operator
  \"!=\" @operator
  \"*\" @operator
  \"&\" @operator
  \"&&\" @operator
  \"+\" @operator
  \"++\" @operator
  \"+=\" @operator
  \"<\" @operator
  \"==\" @operator
  \">\" @operator
  \"||\" @operator

  ; Delimiters
  \".\" @delimiter
  \";\" @delimiter

  ; Literals
  (number_literal) @number
  (char_literal) @number
  (string_literal) @string
  (system_lib_string) @string
  (null) @constant

  ; Identifiers
  (identifier) @variable
  
  ((identifier) @constant
   (#match? @constant \"^[A-Z][A-Z\\\\d_]*$\"))

  (field_identifier) @property
  (statement_identifier) @label
  (type_identifier) @type
  (primitive_type) @type
  (sized_type_specifier) @type

  ; Functions
  (call_expression
    function: (identifier) @function)
    
  (call_expression
    function: (field_expression
      field: (field_identifier) @function))
      
  (function_declarator
    declarator: (identifier) @function)
    
  (preproc_function_def
    name: (identifier) @function.special)
")

(define c-mode-map (make-sparse-keymap))

(define (c-mode-setup-treesitter)
  "Initialize tree-sitter for C mode."
  (when (treesit-language-available? 'c)
    ;; Create parser for current buffer
    (if (treesit-parser-create 'c)
        (begin
          ;; Set the highlight query
          (if (treesit-set-highlight-query! c-treesit-highlight-query)
              (begin
                ;; Apply highlights
                (treesit-apply-highlights)
                )
              ))
        )))

(define-derived-mode c-mode #f "C"
  "Major mode for editing C code."
  (use-local-map c-mode-map)
  (setq-local 'tab-width 4)
  ;; Initialize tree-sitter if available
  (c-mode-setup-treesitter))

