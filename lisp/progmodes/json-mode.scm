;;; json-mode.scm --- A major-mode for editing json

;;; Commentary:

;;

;;; Code:


(define json-treesit-highlight-query
  "
   (string) @string

   (pair
    key: (_) @property)

   (escape_sequence) @string.escape

   (number) @number

   [
     (null)
     (true)
     (false)
   ] @constant.builtin


   (comment) @comment

   ([\"[\" \"]\" \"{\" \"}\"]) @bracket

   ([ \",\" \":\" ]) @delimiter

   (ERROR) @error


")



(define json-mode-map (make-sparse-keymap))

(define (json-mode-setup-treesitter)
  "Initialize tree-sitter for Json mode."
  (when (treesit-language-available? 'json)
    ;; Create parser for current buffer
    (if (treesit-parser-create 'json)
        (begin
          ;; Set the highlight query
          (if (treesit-set-highlight-query! json-treesit-highlight-query)
              (begin
                ;; Apply highlights
                (treesit-apply-highlights)
                )
              ))
        )))

(define-derived-mode json-mode prog-mode "Json"
  "Major mode for editing Json."
  (use-local-map json-mode-map)
  (setq-local tab-width 4)
  ;; Initialize tree-sitter if available
  (json-mode-setup-treesitter))
