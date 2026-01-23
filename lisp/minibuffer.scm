;;; minibuffer.scm --- Minibuffer and completion functions

;;; Commentary:

;; Names with "--" are for functions and variables that are meant to be for
;; internal use only.

;;; Code:

(define completion-show-help #f)
(set-var-doc! completion-show-help
"Non-nil means show help message in *Completions* buffer.")

(define history-length 100)
(set-var-doc! history-length
"Maximum length of history lists before truncation takes place.

A number means truncate to that length; truncation deletes old
elements, and is done just after inserting a new element.
A value of #t means no truncation.

This variable only affects history lists that don't specify their own
maximum lengths.  Setting the history-length property of a history
variable overrides this default.")


;; TODO Support me!
(define minibuffer-completion-auto-choose #f)
(set-var-doc! minibuffer-completion-auto-choose
"Non-nil means to automatically insert completions to the minibuffer.
When non-nil, then `minibuffer-next-completion' and
`minibuffer-previous-completion' will insert the completion
selected by these commands to the minibuffer.")

(define minibuffer-mode-map (make-sparse-keymap))
(define-key minibuffer-mode-map "TAB" minibuffer-complete)
(define-key minibuffer-mode-map "RET" minibuffer-complete-and-exit)
(define-key minibuffer-mode-map "M-RET" minibuffer-choose-completion)
(define-key minibuffer-mode-map "M-<up>" minibuffer-previous-completion)
(define-key minibuffer-mode-map "M-<down>" minibuffer-next-completion)
(define-key minibuffer-mode-map "C-p" minibuffer-previous-completion)
(define-key minibuffer-mode-map "C-n" minibuffer-next-completion)
(define-key minibuffer-mode-map "M-p" previous-history-element)
(define-key minibuffer-mode-map "M-n" next-history-element)

(define-derived-mode minibuffer-mode nil "Minibuffer"
  "Major mode used for active minibuffers.
For customizing this mode, it is better to use `minibuffer-setup-hook'
and `minibuffer-exit-hook' rather than the mode hook of this mode."
  (use-local-map minibuffer-mode-map))


(define (completing-read prompt collection)
  "Read a string in the minibuffer, with completion.
PROMPT is the prompt string.
COLLECTION is a list of possible completions (strings or symbols)."
  ;; Use 'minibuffer-history as default history for completing-read
  (read-from-minibuffer-with-completion prompt "" collection #f 'minibuffer-history))

(define (with-minibuffer-completions-window thunk)
  "Execute THUNK with *Completions* buffer as current buffer."
  (let ((completions-buf (get-buffer "*Completions*"))
        (old-buf (current-buffer)))
    (if (not completions-buf)
        (message "No completions window")
        (begin
          (set-buffer completions-buf)
          (thunk)
          (set-buffer old-buf)))))

;; For load-theme command
(define (read-theme prompt)
  "Read a theme name with completion."
  (let ((themes (custom-available-themes)))
    (if (null? themes)
        (begin
          (message "No themes available")
          #f)
        (let ((theme-str (read-from-minibuffer-with-completion
                          prompt "" themes #f 'custom-theme-history)))
          (if (and theme-str (not (string=? theme-str "")))
              (string->symbol theme-str)
              #f)))))
