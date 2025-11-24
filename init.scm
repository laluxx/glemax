;; Define functions with docstrings
(define (my-custom-command)
  "Insert a greeting at point"
  (message "Hello KeyChords from Scheme!"))


(define (my-function number)
  "Insert the best number"
  (message "The best number is: ~a" number))


(define-syntax keychord-bind-with-args
  (syntax-rules ()
    ((_ key func arg ...)
     (keychord-bind key (lambda () (func arg ...))))))

(keychord-bind-with-args "M-m" my-function 10)
(keychord-bind-with-args "C-c f" (lambda () (load user-init-file)))

(keychord-bind "M-z"
               (lambda ()
                 "docstring"
                 (message "Point: ~d" (point))))

;; Bind it - documentation is automatically extracted
(keychord-bind "C-c i" my-custom-command)

(keychord-bind "C-x k" kill-buffer)
(keychord-bind "C-x <left>"  previous-buffer)
(keychord-bind "C-x <right>" next-buffer)
(keychord-bind "C-b" backward-char)
(keychord-bind "C-f" forward-char)
(keychord-bind "C-n" next-line)
(keychord-bind "C-p" previous-line)
(keychord-bind "<left>" backward-char)
(keychord-bind "<right>" forward-char)
(keychord-bind "<down>" next-line)
(keychord-bind "<up>" previous-line)
(keychord-bind "M-f" forward-word)
(keychord-bind "M-b" backward-word)
(keychord-bind "C-<right>" forward-word)
(keychord-bind "C-<left>" backward-word)
(keychord-bind "M-d" kill-word)
(keychord-bind "C-e" end-of-line)
(keychord-bind "C-a" beginning-of-line)
(keychord-bind "M-<" beginning-of-buffer)
(keychord-bind "C-c p" beginning-of-buffer)
(keychord-bind "M->" end-of-buffer)
(keychord-bind "C-c n" end-of-buffer)
(keychord-bind "RET" newline)
(keychord-bind "C-j" newline)
(keychord-bind "C-m" newline)
(keychord-bind "<backspace>" delete-backward-char)
(keychord-bind "C-<backspace>" backward-kill-word)
(keychord-bind "C-d" delete-char)
(keychord-bind "<delete>" delete-char)
(keychord-bind "C-o" open-line)
(keychord-bind "C-M-o" split-line)
(keychord-bind "C-SPC" set-mark-command)
(keychord-bind "C-x C-x" exchange-point-and-mark)
(keychord-bind "S-<backspace>" delete-region)
(keychord-bind "C-w" kill-region)
(keychord-bind "C-y" yank)
(keychord-bind "C-k" kill-line)
(keychord-bind "M-}" forward-paragraph)
(keychord-bind "M-{" backward-paragraph)
(keychord-bind "C-<down>" forward-paragraph)
(keychord-bind "C-<up>" backward-paragraph)
(keychord-bind "C-x 2" split-window-below)
(keychord-bind "C-x 3" split-window-right)
(keychord-bind "C-x 0" delete-window)
(keychord-bind "C-x 1" delete-other-windows)
(keychord-bind "C-x o" other-window)
(keychord-bind "C-x +" balance-windows)
(keychord-bind "C-x ^" enlarge-window)
(keychord-bind "C-l" recenter-top-bottom)
(keychord-bind "C-v" recenter)
(keychord-bind "C-u" universal-argument)
(keychord-bind "C--" negative-argument)
(keychord-bind "C-0" digit-argument)
(keychord-bind "C-1" digit-argument)
(keychord-bind "C-2" digit-argument)
(keychord-bind "C-3" digit-argument)
(keychord-bind "C-4" digit-argument)
(keychord-bind "C-5" digit-argument)
(keychord-bind "C-6" digit-argument)
(keychord-bind "C-7" digit-argument)
(keychord-bind "C-8" digit-argument)
(keychord-bind "C-9" digit-argument)
(keychord-bind "M-x" execute-extended-command)
(keychord-bind "C-g" keyboard-quit)
(keychord-bind "C-x C-e" eval-last-sexp)
(keychord-bind "C-x C-b" eval-buffer)
(keychord-bind "C-x C-r" eval-region)
(keychord-bind "C-t" transpose-chars)
(keychord-bind "M-t" transpose-words)
;; Well.. We need to get arguments right.
;; ..ooor do some maro magic but it would be bad
;; (keychord-bind "C-T" (lambda () (set! prefix-arg -1) (transpose-chars)))
;; (keychord-bind "M-T" (lambda () (set! prefix-arg -1) (transpose-words)))
;; We did it!
(keychord-bind "C-T" (lambda () (transpose-chars -1)))
(keychord-bind "M-T" (lambda () (transpose-words -1)))

(keychord-bind "C-M-n" forward-list)
(keychord-bind "C-M-p" backward-list)
(keychord-bind "C-v" scroll-up-command)
(keychord-bind "M-v" scroll-down-command)
(keychord-bind "C-M-v" scroll-other-window)
(keychord-bind "C-M-S-v" scroll-other-window-down)
(keychord-bind "M-r" move-to-window-line-top-bottom)
(keychord-bind "M-u" upcase-word)
(keychord-bind "M-l" downcase-word)
(keychord-bind "M-c" capitalize-word)
(keychord-bind "C-x C-o" delete-blank-lines)
(keychord-bind "M-m" back-to-indentation)
(keychord-bind "M-^" delete-indentation)
(keychord-bind "C-M-f" forward-sexp)
(keychord-bind "C-M-b" backward-sexp)
(keychord-bind "C-M-k" kill-sexp)



(define (mark-whole-buffer)
  "Put point at beginning and mark at end of buffer."
  (set-mark (buffer-size))
  (beginning-of-buffer))

(keychord-bind "C-x h" mark-whole-buffer)


;; TODO
;; C-M-u   - backward-up-list
;; C-M-d   - down-list

;; M-SPC   - cycle-spacing
;; M-w     - kill-ring-save
;; M-k     - kill-sentence
;; C-M-t   - transpose-sexps
;; C-x C-l - downcase-region
;; C-x C-u - upcase-region

;; C-M-t   - transpose-sexps
;; M-C-t   - transpose-sexps

;; C-x ]   - forward-page
;; C-x [   - backward-page
;; C-x C-p - mark-page


;; (key-binding (kbd "C-n"))


(define (set-var-doc! v doc)
  (set-object-property! v 'documentation doc))

(define (variable-documentation var)
  "variable-documentation var
Return the documentation property associated with `var'."
  (object-property var 'documentation))



(define blink-cursor-mode #t)
(set-var-doc! blink-cursor-mode
"Controls cursor blinking (Blink Cursor mode).

If the value of `blink-cursor-blinks' is positive (10 by default),
the cursor stops blinking after that number of blinks, if Glemax
gets no input during that time.

See also `blink-cursor-interval' and `blink-cursor-delay'.")

(define visible-mark-mode #t)
(set-var-doc! visible-mark-mode
"#t if the mark should be visible.")

(define blink-cursor-blinks 10)
(set-var-doc! blink-cursor-blinks
"How many times to blink before using a solid cursor.
Use 0 or negative value to blink forever.")

(define blink-cursor-interval 0.5)
(set-var-doc! blink-cursor-interval
"Length of cursor blink interval in seconds.")

(define blink-cursor-delay 0.5)
(set-var-doc! blink-cursor-delay
"Seconds of idle time before the first blink of the cursor.
TODO Values smaller than 0.2 sec are treated as 0.2 sec.")



(define electric-pair-mode #t)
(set-var-doc! electric-pair-mode
"Toggle automatic pairing of delimiters (Electric Pair mode).

Electric Pair mode is a global minor mode.  When enabled, typing an
opening delimiter (parenthesis, bracket, etc.) automatically inserts the
corresponding closing delimiter.  If the region is active, the
delimiters are inserted around the region instead.")

(define kill-whole-line #t)
(set-var-doc! kill-whole-line
"If non-false, `kill-line' with no arg at start of line kills the whole line.
This variable also affects `kill-visual-line' in the same way as
it does `kill-line'.")

(define transient-mark-mode #f)
(set-var-doc! transient-mark-mode
"Toggle Transient Mark mode.

Transient Mark mode is a global minor mode.  When enabled, the
region is highlighted with the region face whenever the mark
is active.  The mark is \"deactivated\" after certain non-motion
commands, including those that change the text in the buffer, and
during shift or mouse selection by any unshifted cursor motion
command.

You can also deactivate the mark by typing C-g

Many commands change their behavior when Transient Mark mode is
in effect and the mark is active, by acting on the region instead
of their usual default part of the buffer's text.  Examples of
such commands include `backward-delete-char`.")


(define initial-mark-visible #f)
(set-var-doc! initial-mark-visible
"Non-false means display the mark at the beginning of the buffer initially.
When non-false, the mark is shown at buffer start before it is set manually
via `set-mark-command`.")


(define prefix-arg 1)
(define raw-prefix-arg #f)


(define (point-min)
  "Return the minimum permissible value of point in the current buffer.
This is 0, unless narrowing is in effect."
  0)

(define (point-max)
  "Return the maximum permissible value of point in the current buffer.
This is (buffer-size), unless narrowing is in effect,
in which case it is less."
  (buffer-size))

(define (bob?)
  "Return #t if point is at beginning of buffer."
  (= (point) (point-min)))

(define (eob?)
  "Return #t if point is at end of buffer."
  (= (point) (point-max)))

(define (bol?)
  "Return #t if point is at beginning of line."
  (= (point) (line-beginning-position)))

(define (eol?)
  "Return #t if point is at end of line.
`End of a line' includes point being at the end of the buffer."
  (= (point) (line-end-position)))

(define (region-beginning)
  "Return the integer value of point or mark, whichever is smaller."
  (min (point) (mark)))

(define (region-end)
  "Return the integer value of point or mark, whichever is larger."
  (max (point) (mark)))

(define (region-active?)
  "Return t if Transient Mark mode is enabled and the mark is active."
  (mark-active?))


(define (delete-horizontal-space)
  "Delete all spaces and tabs around point.
With prefix argument, delete them only before point."
  ;; Delete backward
  (while (and (not (bob?))
              (let ((ch (char-before (point))))
                (or (= ch 32) (= ch 9)))) ; space or tab
    (delete-backward-char))
  ;; Delete forward (unless prefix-arg was given)
  (when (= prefix-arg 1)
    (while (and (not (eob?))
                (let ((ch (char-after (point))))
                  (or (= ch 32) (= ch 9)))) ; space or tab
      (delete-char))))

(keychord-bind "M-\\" delete-horizontal-space)







(deftheme 'kaolin-dark "A dark jade theme inspired by Sierra.vim")

(custom-theme-set-faces 'kaolin-dark
  '(default            ((t (:foreground "#e4e4e8" :background "#18181B"))))
  '(mode-line          ((t (:foreground "#babac4" :background "#222225"))))
  '(mode-line-active   ((t (:foreground "#babac4" :background "#222225"))))
  '(mode-line-inactive ((t (:foreground "#545c5e" :background "#222225"))))
  '(window-divider     ((t (                      :background "#2B2B2F"))))
  '(fringe             ((t (:foreground "#e4e4e8" :background "#18181B"))))
  '(cursor             ((t (                      :background "#e4e4e8"))))
  '(visible-mark       ((t (                      :background "#968cc7"))))
)


(deftheme 'kaolin-bubblegum "Kaolin colorful theme with dark blue background")

(custom-theme-set-faces 'kaolin-bubblegum
  '(default            ((t (:foreground "#D4D4D6" :background "#14171E"))))
  '(mode-line          ((t (:foreground "#bebec4" :background "#191D26"))))
  '(mode-line-active   ((t (:foreground "#bebec4" :background "#191D26"))))
  '(mode-line-inactive ((t (:foreground "#454459" :background "#191D26"))))
  '(window-divider     ((t (                      :background "#202430"))))
  '(fringe             ((t (:foreground "#e6e6e8" :background "#14171e"))))
  '(cursor             ((t (                      :background "#D6A0D1"))))
  '(visible-mark       ((t (                      :background "#41b0f3"))))
)


(deftheme 'kaolin-ocean "Dark blue kaolin theme")

(custom-theme-set-faces 'kaolin-ocean
  '(default            ((t (:foreground "#E6E6E8" :background "#1A1A25"))))
  '(mode-line          ((t (:foreground "#bebec4" :background "#252534"))))
  '(mode-line-active   ((t (:foreground "#bebec4" :background "#252534"))))
  '(mode-line-inactive ((t (:foreground "#545c5e" :background "#252534"))))
  '(window-divider     ((t (                      :background "#2f2f43"))))
  '(fringe             ((t (:foreground "#e6e6e8" :background "#1a1a25"))))
  '(cursor             ((t (                      :background "#F2F2F2"))))
  '(visible-mark       ((t (                      :background "#738FD7"))))
)


(deftheme 'kaolin-temple "The terrestrial sphere imbues my spirit")

(custom-theme-set-faces 'kaolin-temple
  '(default            ((t (:foreground "#EEDCC1" :background "#2B2B2F"))))
  '(mode-line          ((t (:foreground "#bebec4" :background "#303035"))))
  '(mode-line-active   ((t (:foreground "#bebec4" :background "#303035"))))
  '(mode-line-inactive ((t (:foreground "#697375" :background "#303035"))))
  '(window-divider     ((t (                      :background "#353b3c"))))
  '(fringe             ((t (:foreground "#EEDCC1" :background "#2B2B2F"))))
  '(cursor             ((t (                      :background "#EEDCC1"))))
  '(visible-mark       ((t (                      :background "#fbaed2"))))
)


(deftheme 'dark+ "ported from equinusocio's VSCode theme, dark+")

(custom-theme-set-faces 'dark+
  '(default            ((t (:foreground "#d4d4d4" :background "#1e1e1e"))))
  '(mode-line          ((t (:foreground "#f4f4f4" :background "#68217A"))))
  '(mode-line-active   ((t (:foreground "#f4f4f4" :background "#68217A"))))
  '(mode-line-inactive ((t (:foreground "#339CDB" :background "#1d1d1d"))))
  '(window-divider     ((t (                      :background "#252526"))))
  '(fringe             ((t (:foreground "#4b474c" :background "#1e1e1e"))))
  '(cursor             ((t (                      :background "#237AD3"))))
  '(visible-mark       ((t (                      :background "#a9a9a9"))))
)


(deftheme 'dark-one "inspired by Atom One Dark")

(custom-theme-set-faces 'dark-one
  '(default            ((t (:foreground "#BBC2CF" :background "#282C34"))))
  '(mode-line          ((t (:foreground "#bbc2cf" :background "#1d2026"))))
  '(mode-line-active   ((t (:foreground "#bbc2cf" :background "#1d2026"))))
  '(mode-line-inactive ((t (:foreground "#5B6268" :background "#21242b"))))
  '(window-divider     ((t (                      :background "#191b20"))))
  '(fringe             ((t (:foreground "#3f444a" :background "#282c34"))))
  '(cursor             ((t (                      :background "#51AFEF"))))
  '(visible-mark       ((t (                      :background "#c678dd"))))
)


(deftheme 'city-lights "inspired by Atom's City Lights theme")

(custom-theme-set-faces 'city-lights
  '(default            ((t (:foreground "#A0B3C5" :background "#1D252C"))))
  '(mode-line          ((t (:foreground "#A0B3C5" :background "#181f25"))))
  '(mode-line-active   ((t (:foreground "#A0B3C5" :background "#181f25"))))
  '(mode-line-inactive ((t (:foreground "#56697A" :background "#1D252C"))))
  '(window-divider     ((t (                      :background "#0b0e11"))))
  '(fringe             ((t (:foreground "#384551" :background "#1D252C"))))
  '(cursor             ((t (                      :background "#51AFEF"))))
  '(visible-mark       ((t (                      :background ""))))
)


(deftheme 'molokai "inspired by Tomas Restrepo's Molokai")

(custom-theme-set-faces 'molokai
  '(default            ((t (:foreground "#D6D6D4" :background "#1C1E1F"))))
  '(mode-line          ((t (:foreground "#d6d6d4" :background "#2d2e2e"))))
  '(mode-line-active   ((t (:foreground "#d6d6d4" :background "#2d2e2e"))))
  '(mode-line-inactive ((t (:foreground "#4e4e4e" :background "#171819"))))
  '(window-divider     ((t (                      :background "#323435"))))
  '(fringe             ((t (:foreground "#4e4e4e" :background "#1c1e1f"))))
  '(cursor             ((t (                      :background "#FB2874"))))
  '(visible-mark       ((t (                      :background ""))))
)


(deftheme 'monokai-ristretto "Port of Monokai Ristretto")

(custom-theme-set-faces 'monokai-ristretto
  '(default            ((t (:foreground "#fff1f3" :background "#2c2525"))))
  '(mode-line          ((t (:foreground "#fff1f3" :background "#403838"))))
  '(mode-line-active   ((t (:foreground "#fff1f3" :background "#403838"))))
  '(mode-line-inactive ((t (:foreground "#fff1f3" :background "#2c2525"))))
  '(window-divider     ((t (                      :background "#413a3a"))))
  '(fringe             ((t (:foreground "#5b5353" :background "#2c2525"))))
  '(cursor             ((t (                      :background "#fff1f3"))))
  '(visible-mark       ((t (                      :background "#adda78"))))
)


(deftheme 'nord "dark variant of Nord")

(custom-theme-set-faces 'nord
  '(default            ((t (:foreground "#ECEFF4" :background "#2E3440"))))
  '(mode-line          ((t (:foreground "#ECEFF4" :background "#292e39"))))
  '(mode-line-active   ((t (:foreground "#ECEFF4" :background "#292e39"))))
  '(mode-line-inactive ((t (:foreground "#9099AB" :background "#292e39"))))
  '(window-divider     ((t (                      :background "#1c2028"))))
  '(fringe             ((t (:foreground "#8FBCBB" :background "#2E3440"))))
  '(cursor             ((t (                      :background "#81A1C1"))))
  '(visible-mark       ((t (                      :background "#8FBCBB"))))
)


(deftheme 'modus-vivendi "Elegant, highly legible theme with a black background")


(custom-theme-set-faces 'modus-vivendi
  '(default            ((t (:foreground "#ffffff" :background "#000000"))))
  '(mode-line          ((t (:foreground "#ffffff" :background "#505050"))))
  '(mode-line-active   ((t (:foreground "#ffffff" :background "#505050"))))
  '(mode-line-inactive ((t (:foreground "#969696" :background "#2d2d2d"))))
  '(window-divider     ((t (                      :background "#646464"))))
  '(fringe             ((t (:foreground "#ffffff" :background "#1e1e1e"))))
  '(cursor             ((t (                      :background "#ffffff"))))
  '(visible-mark       ((t (                      :background "#feacd0"))))
)


(deftheme 'test-theme "test theme that only sets red foreground")

(custom-theme-set-faces 'test-theme
  '(default            ((t (:foreground "#FF0000" :background "#882200"))))
  '(cursor             ((t (:inherit default))))
)



(load-theme 'dark-one)



;; defvar - define a variable with a default value
(define-syntax defvar
  (syntax-rules ()
    ((_ var default-value)
     (set-default! 'var default-value))
    ((_ var default-value docstring)
     (set-default! 'var default-value))))

;; Define a buffer-local variable (automatically buffer-local in all buffers)
(define-syntax defvar-local
  (syntax-rules ()
    ((_ var default-value)
     (begin
       (set-default! 'var default-value)
       (make-variable-buffer-local 'var)))
    ((_ var default-value docstring)
     (begin
       (set-default! 'var default-value)
       (make-variable-buffer-local 'var)))))

;; setq-local - set buffer-local value
(define (setq-local symbol value)
  (when (not (local-variable? symbol))
    (make-local-variable symbol))
  (set symbol value))


(defvar-local truncate-lines #f
  "Non-nil means truncate lines in this buffer.
When truncating is off, long lines are folded.")


;; TODO Make draw_buffer respect this
(define (toggle-truncate-lines)
  "Toggle truncating of long lines for the current buffer."
  (let ((current-val (buffer-local-value 'truncate-lines (current-buffer))))
    (setq 'truncate-lines (not current-val))
    (message "Truncate long lines ~a" 
             (if (not current-val) "enabled" "disabled"))))

(keychord-bind "C-x x t" toggle-truncate-lines)
