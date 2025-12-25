;;; zig-mode.scm --- major mode for the Zig programming language

;;; Commentary:

;; A major mode for the Zig programming languages.

;;; Code:

(define zig-treesit-highlight-query
  "(identifier) @variable

(parameter
  name: (identifier) @variable.parameter)

(payload
  (identifier) @variable.parameter)

(parameter
  type: (identifier) @type)

((identifier) @type
  (#lua-match? @type \"^[A-Z_][a-zA-Z0-9_]*\"))

(variable_declaration
  (identifier) @type
  \"=\"
  [
    (struct_declaration)
    (enum_declaration)
    (union_declaration)
    (opaque_declaration)
  ])

[
  (builtin_type)
  \"anyframe\"
] @type.builtin

((identifier) @constant
  (#lua-match? @constant \"^[A-Z][A-Z_0-9]+$\"))

[
  \"null\"
  \"unreachable\"
  \"undefined\"
] @constant.builtin

(field_expression
  .
  member: (identifier) @constant)

(enum_declaration
  (container_field
    type: (identifier) @constant))

(block_label
  (identifier) @label)

(break_label
  (identifier) @label)

(field_initializer
  .
  (identifier) @variable.member)

(field_expression
  (_)
  member: (identifier) @variable.member)

(container_field
  name: (identifier) @variable.member)

(initializer_list
  (assignment_expression
    left: (field_expression
      .
      member: (identifier) @variable.member)))

(builtin_identifier) @function.builtin

(call_expression
  function: (identifier) @function.call)

(call_expression
  function: (field_expression
    member: (identifier) @function.call))

(function_declaration
  name: (identifier) @function)

(variable_declaration
  (identifier) @module
  (builtin_function
    (builtin_identifier) @keyword.import
    (#any-of? @keyword.import \"@import\" \"@cImport\")))

[
  \"c\"
  \"...\"
] @variable.builtin

((identifier) @variable.builtin
  (#eq? @variable.builtin \"_\"))

(calling_convention
  (identifier) @variable.builtin)

[
  \"asm\"
  \"defer\"
  \"errdefer\"
  \"test\"
  \"error\"
  \"const\"
  \"var\"
] @keyword

[
  \"struct\"
  \"union\"
  \"enum\"
  \"opaque\"
] @keyword.type

[
  \"async\"
  \"await\"
  \"suspend\"
  \"nosuspend\"
  \"resume\"
] @keyword.coroutine

\"fn\" @keyword.function

[
  \"and\"
  \"or\"
  \"orelse\"
] @keyword.operator

\"return\" @keyword.return

[
  \"if\"
  \"else\"
  \"switch\"
] @keyword.conditional

[
  \"for\"
  \"while\"
  \"break\"
  \"continue\"
] @keyword.repeat

[
  \"usingnamespace\"
  \"export\"
] @keyword.import

[
  \"try\"
  \"catch\"
] @keyword.exception

[
  \"volatile\"
  \"allowzero\"
  \"noalias\"
  \"addrspace\"
  \"align\"
  \"callconv\"
  \"linksection\"
  \"pub\"
  \"inline\"
  \"noinline\"
  \"extern\"
  \"comptime\"
  \"packed\"
  \"threadlocal\"
] @keyword.modifier

[
  \"=\"
  \"*=\"
  \"*%=\"
  \"*|=\"
  \"/=\"
  \"%=\"
  \"+=\"
  \"+%=\"
  \"+|=\"
  \"-=\"
  \"-%=\"
  \"-|=\"
  \"<<=\"
  \"<<|=\"
  \">>=\"
  \"&=\"
  \"^=\"
  \"|=\"
  \"!\"
  \"~\"
  \"-\"
  \"-%\"
  \"&\"
  \"==\"
  \"!=\"
  \">\"
  \">=\"
  \"<=\"
  \"<\"
  \"&\"
  \"^\"
  \"|\"
  \"<<\"
  \">>\"
  \"<<|\"
  \"+\"
  \"++\"
  \"+%\"
  \"-%\"
  \"+|\"
  \"-|\"
  \"*\"
  \"/\"
  \"%\"
  \"**\"
  \"*%\"
  \"*|\"
  \"||\"
  \".*\"
  \".?\"
  \"?\"
  \"..\"
] @operator

(character) @character

([
  (string)
  (multiline_string)
] @string
  (#set! \"priority\" 95))

(integer) @number

(float) @number.float

(boolean) @boolean

(escape_sequence) @string.escape

[
  \"[\"
  \"]\"
  \"(\"
  \")\"
  \"{\"
  \"}\"
] @punctuation.bracket

[
  \";\"
  \".\"
  \",\"
  \":\"
  \"=>\"
  \"->\"
] @punctuation.delimiter

(payload
  \"|\" @punctuation.bracket)

(comment) @comment @spell

((comment) @comment.documentation
  (#lua-match? @comment.documentation \"^//!\"))")

(define zig-mode-map (make-sparse-keymap))

(define (zig-mode-setup-treesitter)
  "Initialize tree-sitter for Zig mode."
  (when (treesit-language-available? 'zig)
    (message "Setting up tree-sitter for Zig...")
    
    ;; Create parser for current buffer
    (if (treesit-parser-create 'zig)
        (begin
          ;; Set the highlight query
          (if (treesit-set-highlight-query! zig-treesit-highlight-query)
              (begin
                ;; Apply highlights
                (treesit-apply-highlights)
                (message "Tree-sitter highlighting enabled"))
              (message "Failed to set highlight query")))
        (message "Failed to create tree-sitter parser"))))

(define-derived-mode zig-mode prog-mode "Zig"
  "A major mode for the Zig programming language."
  (use-local-map zig-mode-map)
  (setq-local tab-width 4)
  ;; Initialize tree-sitter if available
  (zig-mode-setup-treesitter)
  (message "Zig mode enabled"))
