;;; python-mode.scm --- Python editing mode

;;; Commentary:

;; Major mode for editing Python files

;;; Code:

(define python-treesit-highlight-query
  "
; Variable assignments
(assignment
  left: (identifier) @variable)

(assignment
  left: (pattern_list
    (identifier) @variable))

; Augmented assignments (+=, -=, etc.)
(augmented_assignment
  left: (identifier) @variable)

; Named expressions (walrus operator :=)
(named_expression
  name: (identifier) @variable)

; As pattern (with statements and exception handling)
(as_pattern
  alias: (as_pattern_target) @variable)

; Identifier naming conventions
((identifier) @constructor
 (#match? @constructor \"^[A-Z]\"))

((identifier) @constant
 (#match? @constant \"^[A-Z][A-Z_]*$\"))

; Function calls
(decorator) @function

(decorator
  (identifier) @function)

(call
  function: (identifier) @function)

; Builtin functions
((call
  function: (identifier) @function.builtin)
 (#match?
   @function.builtin
   \"^(abs|all|any|ascii|bin|bool|breakpoint|bytearray|bytes|callable|chr|classmethod|compile|complex|delattr|dict|dir|divmod|enumerate|eval|exec|filter|float|format|frozenset|getattr|globals|hasattr|hash|help|hex|id|input|int|isinstance|issubclass|iter|len|list|locals|map|max|memoryview|min|next|object|oct|open|ord|pow|print|property|range|repr|reversed|round|set|setattr|slice|sorted|staticmethod|str|sum|super|tuple|type|vars|zip|__import__)$\"))

; Function definitions
(function_definition
  name: (identifier) @function)

(type (identifier) @type)

; Literals
[
  (none)
  (true)
  (false)
] @constant.builtin

[
  (integer)
  (float)
] @number

(comment) @comment
(string) @string
(escape_sequence) @string.escape

(interpolation
  \"{\" @punctuation.special
  \"}\" @punctuation.special) @embedded

[
  \"-\"
  \"-=\"
  \"!=\"
  \"*\"
  \"**\"
  \"**=\"
  \"*=\"
  \"/\"
  \"//\"
  \"//=\"
  \"/=\"
  \"&\"
  \"&=\"
  \"%\"
  \"%=\"
  \"^\"
  \"^=\"
  \"+\"
  \"->\"
  \"+=\"
  \"<\"
  \"<<\"
  \"<<=\"
  \"<=\"
  \"<>\"
  \"=\"
  \":=\"
  \"==\"
  \">\"
  \">=\"
  \">>\"
  \">>=\"
  \"|\"
  \"|=\"
  \"~\"
  \"@=\"
  \"and\"
  \"or\"
  \"is not\"
  \"not in\"
] @operator

[
  \"as\"
  \"assert\"
  \"async\"
  \"await\"
  \"break\"
  \"class\"
  \"continue\"
  \"def\"
  \"del\"
  \"elif\"
  \"else\"
  \"except\"
  \"exec\"
  \"finally\"
  \"for\"
  \"from\"
  \"global\"
  \"if\"
  \"import\"
  \"in\"
  \"is\"
  \"lambda\"
  \"nonlocal\"
  \"not\"
  \"pass\"
  \"print\"
  \"raise\"
  \"return\"
  \"try\"
  \"while\"
  \"with\"
  \"yield\"
  \"match\"
  \"case\"
] @keyword
")


(define python-mode-map (make-sparse-keymap))

(define (python-mode-setup-treesitter)
  "Initialize tree-sitter for Python mode."
  (when (treesit-language-available? 'python)
    ;; Create parser for current buffer
    (if (treesit-parser-create 'python)
        (begin
          ;; Set the highlight query
          (if (treesit-set-highlight-query! python-treesit-highlight-query)
              (begin
                ;; Apply highlights
                (treesit-apply-highlights)
                )
              ))
        )))

(define-derived-mode python-mode prog-mode "Python"
  "Major mode for editing Python code."
  (use-local-map python-mode-map)
  (setq-local 'tab-width 2)
  ;; Initialize tree-sitter if available
  (python-mode-setup-treesitter))
