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
  (true) @constant
  (false) @constant
  
  ; Types
  (type_identifier) @type
  (primitive_type) @type
  (sized_type_specifier) @type
  
  ; Struct, union, and enum names
  (struct_specifier
    name: (type_identifier) @type)
  
  (union_specifier
    name: (type_identifier) @type)
  
  (enum_specifier
    name: (type_identifier) @type)
  
  ; Enumerators
  (enumerator
    name: (identifier) @constant)
  
  ; Function definitions
  (function_definition
    declarator: (function_declarator
      declarator: (identifier) @function))
  
  (function_definition
    declarator: (pointer_declarator
      declarator: (function_declarator
        declarator: (identifier) @function)))
  
  ; Macro function definitions
  (preproc_function_def
    name: (identifier) @function)
  
  ; Variable declarations - simple
  (declaration
    declarator: (identifier) @variable)
  
  ; Variable declarations - with initializer
  (declaration
    declarator: (init_declarator
      declarator: (identifier) @variable))
  
  ; Variable declarations - pointers
  (declaration
    declarator: (pointer_declarator
      declarator: (identifier) @variable))
  
  (declaration
    declarator: (init_declarator
      declarator: (pointer_declarator
        declarator: (identifier) @variable)))
  
  ; Variable declarations - arrays
  (declaration
    declarator: (array_declarator
      declarator: (identifier) @variable))
  
  (declaration
    declarator: (init_declarator
      declarator: (array_declarator
        declarator: (identifier) @variable)))
  
  ; Parameter declarations
  (parameter_declaration
    declarator: (identifier) @variable)
  
  (parameter_declaration
    declarator: (pointer_declarator
      declarator: (identifier) @variable))
  
  (parameter_declaration
    declarator: (array_declarator
      declarator: (identifier) @variable))
  
  ; Field declarations in structs/unions - simple
  (field_declaration
    declarator: (field_identifier) @variable)
  
  ; Field declarations - pointers
  (field_declaration
    declarator: (pointer_declarator
      declarator: (field_identifier) @variable))
  
  ; Field declarations - arrays
  (field_declaration
    declarator: (array_declarator
      declarator: (field_identifier) @variable))
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

(define-derived-mode c-mode prog-mode "C"
  "Major mode for editing C code."
  (use-local-map c-mode-map)
  (setq-local tab-width 4)
  ;; Initialize tree-sitter if available
  (c-mode-setup-treesitter))

