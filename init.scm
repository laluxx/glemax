(keymap-global-set "C-x k" kill-buffer)
(keymap-global-set "C-x b" switch-to-buffer)
(keymap-global-set "C-x <left>"  previous-buffer)
(keymap-global-set "C-x <right>" next-buffer)
(keymap-global-set "C-b" backward-char)
(keymap-global-set "C-f" forward-char)
(keymap-global-set "C-n" next-line)
(keymap-global-set "C-p" previous-line)
(keymap-global-set "<left>" backward-char)
(keymap-global-set "<right>" forward-char)
(keymap-global-set "<down>" next-line)
(keymap-global-set "<up>" previous-line)
(keymap-global-set "M-f" forward-word)
(keymap-global-set "M-b" backward-word)
(keymap-global-set "C-<right>" forward-word)
(keymap-global-set "C-<left>" backward-word)
(keymap-global-set "M-d" kill-word)
(keymap-global-set "C-e" end-of-line)
(keymap-global-set "C-a" beginning-of-line)
(keymap-global-set "C-A" beginning-of-visual-line)
(keymap-global-set "C-E" end-of-visual-line)
(keymap-global-set "M-<" beginning-of-buffer)
(keymap-global-set "C-c p" beginning-of-buffer)
(keymap-global-set "M->" end-of-buffer)
(keymap-global-set "C-c n" end-of-buffer)
(keymap-global-set "RET" newline)
(keymap-global-set "C-j" newline)
(keymap-global-set "C-m" newline)
(keymap-global-set "<backspace>" delete-backward-char)
(keymap-global-set "C-<backspace>" backward-kill-word)
(keymap-global-set "C-d" delete-char)
(keymap-global-set "<delete>" delete-char)
(keymap-global-set "C-o" open-line)
(keymap-global-set "C-M-o" split-line)
(keymap-global-set "C-SPC" set-mark-command)
(keymap-global-set "C-x C-x" exchange-point-and-mark)
(keymap-global-set "S-<backspace>" delete-region)
(keymap-global-set "C-w" kill-region)
(keymap-global-set "M-w" copy-region-as-kill)
(keymap-global-set "C-y" yank)
(keymap-global-set "C-k" kill-line)
(keymap-global-set "M-}" forward-paragraph)
(keymap-global-set "M-{" backward-paragraph)
(keymap-global-set "C-<down>" forward-paragraph)
(keymap-global-set "C-<up>" backward-paragraph)
(keymap-global-set "C-x 2" split-window-below)
(keymap-global-set "C-x 3" split-window-right)
(keymap-global-set "C-x 0" delete-window)
(keymap-global-set "C-x 1" delete-other-windows)
(keymap-global-set "C-x o" other-window)
(keymap-global-set "C-x +" balance-windows)
(keymap-global-set "C-x w -" fit-window-to-buffer)
(keymap-global-set "C-x ^" enlarge-window)
(keymap-global-set "C-l" recenter-top-bottom)
(keymap-global-set "C-v" recenter)
(keymap-global-set "C-u" universal-argument)
(keymap-global-set "C--" negative-argument)
(keymap-global-set "C-0" digit-argument)
(keymap-global-set "C-1" digit-argument)
(keymap-global-set "C-2" digit-argument)
(keymap-global-set "C-3" digit-argument)
(keymap-global-set "C-4" digit-argument)
(keymap-global-set "C-5" digit-argument)
(keymap-global-set "C-6" digit-argument)
(keymap-global-set "C-7" digit-argument)
(keymap-global-set "C-8" digit-argument)
(keymap-global-set "C-9" digit-argument)
(keymap-global-set "M-x" execute-extended-command)
(keymap-global-set "M-:" eval-expression)
(keymap-global-set "C-g" keyboard-quit)
(keymap-global-set "C-x C-e" eval-last-sexp)
(keymap-global-set "C-x C-b" eval-buffer)
(keymap-global-set "C-x C-r" eval-region)
(keymap-global-set "C-t" transpose-chars)
(keymap-global-set "M-t" transpose-words)
(keymap-global-set "C-T" (lambda () (transpose-chars -1)))
(keymap-global-set "M-T" (lambda () (transpose-words -1)))
(keymap-global-set "C-M-n" forward-list)
(keymap-global-set "C-M-p" backward-list)
(keymap-global-set "C-v" scroll-up-command)
(keymap-global-set "M-v" scroll-down-command)
(keymap-global-set "C-M-v" scroll-other-window)
(keymap-global-set "C-M-S-v" scroll-other-window-down)
(keymap-global-set "M-r" move-to-window-line-top-bottom)
(keymap-global-set "M-u" upcase-word)
(keymap-global-set "M-l" downcase-word)
(keymap-global-set "M-c" capitalize-word)
(keymap-global-set "C-x C-o" delete-blank-lines)
(keymap-global-set "M-m" back-to-indentation)
(keymap-global-set "M-^" delete-indentation)
(keymap-global-set "C-M-f" forward-sexp)
(keymap-global-set "C-M-b" backward-sexp)
(keymap-global-set "C-M-k" kill-sexp)
(keymap-global-set "C-M-SPC" mark-sexp)
(keymap-global-set "C-M-e" end-of-defun)
(keymap-global-set "C-M-a" beginning-of-defun)
(keymap-global-set "C-M-d" treesit-debug-tree)
(keymap-global-set "C-x C-q" read-only-mode)
(keymap-global-set "C-x C-s" save-buffer)
(keymap-global-set "C-x C-f" find-file)
(keymap-global-set "C-q" quoted-insert)
(keymap-global-set "C-S-o" duplicate-dwim)
(keymap-global-set "C-x ]" forward-page)
(keymap-global-set "C-x [" backward-page)
(keymap-global-set "C-x C-p" mark-page)
(keymap-global-set "C-x l" count-lines-page)
(keymap-global-set "C-h C-g" garbage-collect)
(keymap-global-set "C-h c" describe-key-briefly)
(keymap-global-set "C-M-c" exit-recursive-edit)


(define (setup-self-insert-keys)
  "Bind all printable characters to self-insert-command in global keymap."

  ;; Printable ASCII characters (space through tilde)
  (let loop ((code 32))  ; Start from space (ASCII 32)
    (when (<= code 126)  ; Up to ~ (ASCII 126)
      (keymap-global-set (string (integer->char code)) self-insert-command)
      (loop (+ code 1))))

  ;; Common extended ASCII / Latin-1 printable characters (160-255)
  ;; These include accented characters, currency symbols, etc.
  (let loop ((code 160))
    (when (<= code 255)
      (keymap-global-set (string (integer->char code)) self-insert-command)
      (loop (+ code 1))))

  ;; TAB as self-insert (some modes override this for indentation)
  (keymap-global-set "TAB" self-insert-command)
  (keymap-global-set "SPC" self-insert-command))

(setup-self-insert-keys)

(define post-self-insert-hook '())

(define wide-newline-cursor t)
(set-var-doc! wide-newline-cursor
"When non-nil the cursor will be as wide
as the widest character on the line on
newline position.")




(define this-command)
(set-var-doc! this-command
"The command now being executed.
The command can set this variable; whatever is put here
will be in `last-command' during the following command.")

(define this-command-event)
(set-var-doc! this-command-event
"Input event of `this-command'.")

(define last-command)
(set-var-doc! last-command
"The last command executed.
Normally a symbol with a function definition, but can be whatever was found
in the keymap, or whatever the variable `this-command' was set to by that
command.

The value `mode-exit' is special; it means that the previous command
read an event that told it to exit, and it did so and unread that event.
In other words, the present command is the event that made the previous
command exit.

The value `kill-region' is special; it means that the previous command
was a kill command.

`last-command' has a separate binding for each terminal device.
See Info node `(elisp)Multiple Terminals'.")

(define last-command-event)
(set-var-doc! last-command-event
"Input event of the last key sequence that called a command.")


(define (mark-whole-buffer)
  "Put point at beginning and mark at end of buffer."
  (set-mark (buffer-size))
  (beginning-of-buffer))

(keymap-global-set "C-x h" mark-whole-buffer)

;; TODO
;; C-M-u   - backward-up-list
;; C-M-d   - down-list
;; C-S-M-o - copy-from-above-command

;; M-SPC   - cycle-spacing
;; M-k     - kill-sentence
;; C-M-t   - transpose-sexps
;; C-x C-l - downcase-region/downcase-dwim
;; C-x C-u - upcase-region/upcase-dwim

;; C-M-t   - transpose-sexps
;; M-C-t   - transpose-sexps

;;; Completion

(defvar-local default-directory "~/"
  "Name of default directory of current buffer.
It should be an absolute directory name; on GNU and Unix systems,
these names start with \"/\" or \"~\" and end with \"/\".
To interactively change the default directory, use the command `cd'.")

(define (file-name-directory path)
  "Return the directory part of PATH, or #f if none."
  (let ((slash (string-rindex path #\/)))
    (if slash
        (substring path 0 (+ slash 1))
        #f)))

(define (file-name-nondirectory path)
  "Return the filename part of PATH (after last slash)."
  (let ((slash (string-rindex path #\/)))
    (if slash
        (substring path (+ slash 1) (string-length path))
        path)))

(define (file-exists-p path)
  (let ((expanded (expand-file-name path)))
    (file-exists? expanded)))

(define (file-directory-p path)
  (let ((expanded (expand-file-name path)))
    (and (file-exists? expanded)
         (eq? 'directory (stat:type (stat expanded))))))

(define (expand-file-name path)
  "Expand ~ in PATH."
  (cond
   ((string-prefix? "~/" path)
    (string-append (getenv "HOME") (substring path 1)))
   ((string=? path "~")
    (getenv "HOME"))
   (else path)))

(define (abbreviate-file-name path)
  "Replace home directory prefix with ~."
  (let ((home (getenv "HOME")))
    (if (and home (string-prefix? home path))
        (string-append "~" (substring path (string-length home)))
        path)))

(define (directory-files dir)
  "Return list of filenames in DIR including . and .."
  (let ((d (opendir dir)))
    (if (not d) '()
        (let loop ((entries '()))
          (let ((e (readdir d)))
            (if (eof-object? e)
                (begin (closedir d) entries)
                (loop (cons e entries))))))))

(define (completion--file-name-table string pred action)
  (let* ((dir-part (or (file-name-directory string) ""))
         (prefix   (file-name-nondirectory string))
         (abs-dir  (expand-file-name
                    (if (string=? dir-part "") "." dir-part)))
         (abs-str  (expand-file-name string)))

    (let* ((raw-entries (if (file-directory-p abs-dir)
                            (directory-files abs-dir)
                            '()))
           (display-names
            (let loop ((entries raw-entries) (acc '()))
              (if (null? entries)
                  acc
                  (let ((e (car entries)))
                    (if (or (string=? e ".") (string=? e ".."))
                        (loop (cdr entries) acc)
                        (let* ((full   (string-append
                                        abs-dir
                                        (if (string-suffix? "/" abs-dir) "" "/")
                                        e))
                               (is-dir (file-directory-p full))
                               (name   (if is-dir (string-append e "/") e)))
                          (loop (cdr entries) (cons name acc))))))))
           (matches
            (let loop ((entries display-names) (acc '()))
              (if (null? entries)
                  acc
                  (loop (cdr entries)
                        (if (string-prefix-ci? prefix (car entries))
                            (cons (car entries) acc)
                            acc)))))
           (sorted
            (sort matches
                  (lambda (a b)
                    (let ((ka (if (string-prefix? "." a) 1 0))
                          (kb (if (string-prefix? "." b) 1 0)))
                      (if (= ka kb)
                          (string<? a b)
                          (< ka kb)))))))

      (cond
       ((eq? action #t)
        ;; Return bare names for display only
        sorted)

       ((eq? action #f)
        (cond
         ((null? sorted)
          (if (file-directory-p abs-str) string #f))
         ((null? (cdr sorted))
          (string-append dir-part (car sorted)))
         (else
          (string-append dir-part
                         (let loop ((pfx (car sorted))
                                    (rest (cdr sorted)))
                           (if (null? rest)
                               pfx
                               (loop (string-common-prefix pfx (car rest))
                                     (cdr rest))))))))

       (else
        (or (file-directory-p abs-str)
            (let loop ((lst sorted))
              (cond
               ((null? lst)                    #f)
               ((string-ci=? (car lst) prefix) #t)
               (else (loop (cdr lst)))))))))))

(define (string-common-prefix a b)
  "Return the longest common prefix of strings A and B."
  (let loop ((i 0))
    (if (or (>= i (string-length a))
            (>= i (string-length b))
            (not (char-ci=? (string-ref a i) (string-ref b i))))
        (substring a 0 i)
        (loop (+ i 1)))))

;; read-file-name — mirrors Emacs read-file-name-default

(define file-name-history '())

(define (read-file-name prompt . args)
  (let* ((dir (if (and (pair? args) (string? (car args)))
                  (car args)
                  default-directory))
         (display-dir (abbreviate-file-name
                       (if (string-suffix? "/" dir) dir
                           (string-append dir "/"))))
         (result
          (read-from-minibuffer-with-completion
           prompt
           display-dir
           completion--file-name-table
           #f
           'file-name-history)))
    ;; If result is a directory, return it as-is with trailing slash
    (let ((expanded (expand-file-name result)))
      (if (and (file-exists? expanded)
               (eq? 'directory (stat:type (stat expanded))))
          (if (string-suffix? "/" expanded)
              expanded
              (string-append expanded "/"))
          result))))

;;; UNDO


(define undo-limit 80000)
(define undo-strong-limit 120000)
(define undo-outer-limit 12000000)
(define undo-ask-before-discard nil)
(define undo-auto-amalgamate-enabled t)

(keymap-global-set "C-/" undo)
(keymap-global-set "C-?" undo-redo)


(define kill-glemax-hook nil)
(set-var-doc! kill-glemax-hook
"Hook run when kill-glemax is called.

Since kill-glemax may be invoked when the terminal is disconnected (or
in other similar situations), functions placed on this hook should not
expect to be able to interact with the user.  To ask for confirmation,
see kill-glemax-query-functions instead.")


(define page-delimiter "^\014")
(set-var-doc! page-delimiter
"Regexp describing line-beginnings that separate pages.")


(define gc-free-space-divisor 3)
(set-var-doc! gc-free-space-divisor
"Free space divisor for the Boehm garbage collector.
Must be a positive integer (minimum 1). Controls how aggressively
the GC runs: lower values collect less often, higher values collect
more often. Boehm's default is 3. Useful range is roughly 1-16.
Set to 1 before loading large files to defer collection.")


;; TODO SUPPORT ME!
(define load-theme-disable-other-themes #t)
(set-var-doc! load-theme-disable-other-themes
"If #t disable all other themes after loading a theme.")


(define eval-display-prompt #t)
(set-var-doc! eval-display-prompt
"If #t display `eval-prompt' before evaluation output.")

(define eval-prompt "=> ")
(set-var-doc! eval-prompt
"String to display before evaluation results when `eval-display-prompt' is #t.")

(define next-screen-context-lines 2)
(set-var-doc! next-screen-context-lines
"Number of lines of continuity when scrolling by screenfuls.")


(define blink-cursor-mode #t)
(set-var-doc! blink-cursor-mode
"Controls cursor blinking (Blink Cursor mode).

If the value of `blink-cursor-blinks' is positive (10 by default),
the cursor stops blinking after that number of blinks, if Glemax
gets no input during that time.

See also `blink-cursor-interval' and `blink-cursor-delay'.")


(define crystal-point-mode #t)
(set-var-doc! crystal-point-mode
"if #t dynamically update the cursor color to match
face foreground color at point.")


(define cursor-in-non-selected-windows nil)
(set-var-doc! cursor-in-non-selected-windows
"Non-nil means show a cursor in non-selected windows.
If nil, only shows a cursor in the selected window.
If t, displays a cursor related to the usual cursor type
(a solid box becomes hollow, a bar becomes a narrower bar).
You can also specify the cursor type as in the `cursor-type' variable.")


(define visible-mark-mode #t)
(set-var-doc! visible-mark-mode
"#t if the mark should be visible.")


(define make-pointer-invisible #t)
(set-var-doc! make-pointer-invisible
"If #t, make mouse pointer invisible while typing.
The pointer becomes visible again when the mouse is moved.

TODO When using this, you might also want to disable highlighting of
clickable text.  See `mouse-highlight'.")


(define make-pointer-invisible-on-keychords #f)
(set-var-doc! make-pointer-invisible-on-keychords
"If #t, make mouse pointer invisible on any keychord invocation.
The pointer becomes visible again when the mouse is moved.

Unlike `make-pointer-invisible', which only hides during typing,
this hides the pointer on all keychords.

TODO When using this, you might also want to disable highlighting of
clickable text.  See `mouse-highlight'.")


(define pointer-visible #t)
(set-var-doc! make-pointer-invisible
"If #t, the mouse pointer is currently visible.")


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


(define duplicate-line-final-position 0)
(set-var-doc! duplicate-line-final-position
"Where to put point after `duplicate-line' or `duplicate-dwim'.
When 0, leave point on the original line.
When 1, move point to the first new line.
When -1, move point to the last new line.
The same column is preserved after moving to a new line.")

(define duplicate-region-final-position 0)
(set-var-doc! duplicate-region-final-position
"Where the region ends up after duplicating a region with `duplicate-region'.
When 0, leave the region in place.
When 1, put the region around the first copy.
When -1, put the region around the last copy.")


(define split-height-threshold 40)
(set-var-doc! split-height-threshold
"Minimum height for splitting windows sensibly.
If this is an integer, `split-window-sensibly' may split a window
vertically only if it has at least this many lines.  If this is
#f, `split-window-sensibly' is not allowed to split a window
vertically.  If, however, a window is the only window on its
frame, or all the other ones are dedicated,
`split-window-sensibly' may split it vertically disregarding the
value of this variable.")

(define split-width-threshold 160)
(set-var-doc! split-width-threshold
"Minimum width for splitting windows sensibly.
If this is an integer, `split-window-sensibly' may split a window
horizontally only if it has at least this many columns.  If this
is #f, `split-window-sensibly' is not allowed to split a window
horizontally.")

(define split-window-preferred-direction 'longest)
(set-var-doc! split-window-preferred-direction
"The first direction tried when Emacs needs to split a window.
This variable controls in which order `split-window-sensibly' will try
to split the window.  That order specially matters when both dimensions
of the frame are long enough to be split according to
`split-width-threshold' and `split-height-threshold'.
If set to `vertical', `split-window-sensibly' tries to split vertically
first and then horizontally.
If set to `horizontal' it does the opposite.
If set to `longest' (the default), the first direction tried depends on
the frame shape: in landscape orientation it will be like `horizontal',
but in portrait it will be like `vertical'.  In other words, the longest
of the two dimension is split first.

If both `split-width-threshold' and `split-height-threshold' cannot be
satisfied, it will fallback to split vertically.

See `split-window-preferred-function' for more control of the splitting
strategy.")


(define split-window-preferred-function 'split-window-sensibly)
(set-var-doc! split-window-preferred-function
"Function called by `display-buffer' routines to split a window.
This function is called with a window as single argument and is
supposed to split that window and return the new window.  If the
window can (or shall) not be split, it is supposed to return nil.
The default is to call the function `split-window-sensibly' which
tries to split the window in a way which seems most suitable.
You can customize the options `split-height-threshold' and/or
`split-width-threshold' in order to have `split-window-sensibly'
prefer either vertical or horizontal splitting.

If you set this to any other function, bear in mind that the
`display-buffer' routines may call this function two times.  The
argument of the first call is the largest window on its frame.
If that call fails to return a live window, the function is
called again with the least recently used window as argument.  If
that call fails too, `display-buffer' will use an existing window
to display its buffer.

The window selected at the time `display-buffer' was invoked is
still selected when this function is called.  Hence you can
compare the window argument with the value of `selected-window'
if you intend to split the selected window instead or if you do
not want to split the selected window.")


(define underline-minimum-offset 1)
(set-var-doc! underline-minimum-offset
"Minimum distance between baseline and underline.
This can improve legibility of underlined text at small font sizes,
particularly when using variable `use-underline-position-properties'
with fonts that specify an UNDERLINE_POSITION relatively close to the
baseline.  The default value is 1.")

(define underline-at-descent-line #f)
(set-var-doc! underline-at-descent-line
"#t means to draw the underline at the same place as the descent line.
(If `line-spacing' is in effect, that moves the underline lower by
that many pixels.)
A value of #f means to draw the underline according to the value of the
variable `use-underline-position-properties', which is usually at the
baseline level.  The default value is nil.")

(define use-underline-position-properties #t)
(set-var-doc! use-underline-position-properties
"#t means make use of UNDERLINE_POSITION font properties.
A value of #f means ignore them.  If you encounter fonts with bogus
UNDERLINE_POSITION font properties, set this to #f.  You can also use
`underline-minimum-offset' to override the font's UNDERLINE_POSITION for
small font display sizes.")

(define next-line-add-newlines #f)
(set-var-doc! next-line-add-newlines
"If #t, `next-line' inserts newline to avoid `end of buffer' error.")

(define line-move-visual #t)
(set-var-doc! line-move-visual
"When #t, `line-move' moves point by visual lines.
This movement is based on where the cursor is displayed on the
screen, instead of relying on buffer contents alone.  It takes
into account variable-width characters and line continuation.
If #f, `line-move' moves point by logical lines.
A non #f setting of `goal-column' overrides the value of this variable
and forces movement by logical lines.
A window that is horizontally scrolled also forces movement by logical
lines.")

(define inhibit-cursor-blink-on-frame-resize #t)
(set-var-doc! inhibit-cursor-blink-on-frame-resize
"When #t, disable cursor blinking while the frame is being resized.
This has only effect if `blink-cursor-mode' is #t.")

(define max-mini-window-height 0.25)
(set-var-doc! max-mini-window-height
"Maximum height for resizing mini-windows (the minibuffer and the echo area).

If a float, it specifies the maximum height in units of the
mini-window frame's height.
If an integer, it specifies the maximum height in units of the
mini-window frame's default font's height.")

(define resize-mini-windows 'grow-only)
(set-var-doc! resize-mini-windows
"How to resize mini-windows (the minibuffer and the echo area).

A value of nil means don't automatically resize mini-windows.
A value of t means resize them to fit the text displayed in them.
A value of `grow-only', the default, means let mini-windows grow only;
they return to their normal size when the minibuffer is closed, or the
echo area becomes empty.

This variable does not affect resizing of the minibuffer window of
minibuffer-only frames.  These are handled by `resize-mini-frames'
only.")

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

(define transient-mark-mode #t)
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

;; I know it should be a minor mode
(define delete-selection-mode #f)
(set-var-doc! delete-selection-mode
"Toggle Delete Selection mode.

When Delete Selection mode is enabled, typed text replaces the selection
if the selection is active.  Otherwise, typed text is just inserted at
point regardless of any selection.

See `delete-selection-helper' and `delete-selection-pre-hook' for
information on adapting behavior of commands in Delete Selection mode.")

(defvar-local tab-width 8
"Distance between tab stops (for display of tab characters), in columns.

This controls the width of a TAB character on display.
The value should be a positive integer.")

(define stretch-cursor nil)
(set-var-doc! stretch-cursor
"Non-nil means draw block cursor as wide as the glyph under it.

For example, if a block cursor is over a tab, it will be drawn as
wide as that tab on the display.")

(define window-resize-pixelwise #f)
(set-var-doc! window-resize-pixelwise
"#t means resize windows pixelwise.
This currently affects the functions: `split-window', `maximize-window',
`minimize-window', `fit-window-to-buffer' and `fit-frame-to-buffer', and
all functions that symmetrically resize a parent window.

Note that when a frame's pixel size is not a multiple of the
frame's character size, at least one window may get resized
pixelwise even if this option is nil.")

(define window-min-width 9)
(set-var-doc! window-min-width
"The minimum total width, in columns, of any window.
The value doesn't accommodate fringes if present.  A value
less than `window-safe-min-width' is ignored.  The value of this
variable is honored when windows are resized or split.

Applications should never rebind this variable.  To resize a
window to a width less than the one specified here, an
application should instead call `window-resize' with a non-nil
IGNORE argument.  In order to have `split-window' make a window
narrower, explicitly specify the SIZE argument of that function.")

(define window-min-height 4)
(set-var-doc! window-min-height
"The minimum total height, in lines, of any window.
The value has to accommodate one text line, a mode and header
line, a horizontal scroll bar and a bottom divider, if present.
A value less than `window-safe-min-height' is ignored.  The value
of this variable is honored when windows are resized or split.

Applications should never rebind this variable.  To resize a
window to a height less than the one specified here, an
application should instead call `window-resize' with a non-nil
IGNORE argument.  In order to have `split-window' make a window
shorter, explicitly specify the SIZE argument of that function.")

(define frame-resize-pixelwise #f)
(set-var-doc! frame-resize-pixelwise
"#t means resize frames pixelwise.
If this option is #f, resizing a frame rounds its sizes to the frame's
current values of `frame-char-height' and `frame-char-width'.  If this
is #t, no rounding occurs, hence frame sizes can increase/decrease
by one pixel.

With some window managers you may have to set this to #t in order
to set the size of a frame in pixels, to maximize frames or to make them
fullscreen.  To resize your initial frame pixelwise, set this option to
a #t value in your init file, non pixelwise resizing doesn't work on wayland.")


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


;; TODO Implement this in C
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

(keymap-global-set "M-\\" delete-horizontal-space)


;; Save current buffer, execute body, restore buffer
(define-syntax save-current-buffer
  (syntax-rules ()
    ((save-current-buffer body ...)
     (let ((saved-buffer (current-buffer)))
       (dynamic-wind
         (lambda () #f)
         (lambda () body ...)
         (lambda () (set-buffer saved-buffer)))))))

;; Execute body with buffer temporarily current
(define-syntax with-current-buffer
  (syntax-rules ()
    ((with-current-buffer buffer-or-name body ...)
     (save-current-buffer
       (set-buffer buffer-or-name)
       body ...))))

(define (messages-buffer)
  "Return the \"*Messages*\" buffer.
If it does not exist, create it and switch it to `messages-buffer-mode'."
  (or (get-buffer "*Messages*")
      (with-current-buffer (get-buffer-create "*Messages*")
        ;; TODO (messages-buffer-mode)
        (current-buffer))))

(define (view-echo-area-messages)
  "View the log of recent echo-area messages: the `*Messages*' buffer.
The number of messages retained in that buffer is specified by
the variable `message-log-max'."
  (let* ((msg-buf (messages-buffer))
         (win (display-buffer msg-buf)))
    ;; Set the window's point to the end of the buffer
    (set-window-point win (buffer-size msg-buf))
    win))

(keymap-global-set "C-h e" view-echo-area-messages)

(defvar-local truncate-lines #f
  "Non-nil means truncate lines in this buffer.
When truncating is off, long lines are folded.")


(defvar-local temporary-goal-column 0
  "Current goal column for vertical motion.
It is the column where point was at the start of the current run
of vertical motion commands.")

(defvar-local goal-column #f
  "Semipermanent goal column for vertical motion, as set by
\\[set-goal-column], or #f.
A non #f setting overrides the variable `line-move-visual', which see.")

(defvar-local line-move-visual-goal-x nil
  "Pixel x goal position for visual line movement. Nil when not in a movement sequence.")


(define (set-goal-column)
  (if raw-prefix-arg
      (begin
        (setq goal-column #f)
        (message "No goal column"))
      (let ((col (current-column)))
        (setq goal-column col)
        (message "Goal column ~a (use C-u C-x C-n to unset it)" col))))

(keymap-global-set "C-x C-n" set-goal-column)


(define (toggle-truncate-lines)
  "Toggle truncating of long lines for the current buffer.
When truncating is off, long lines are folded."
  (let ((current-val (buffer-local-value 'truncate-lines (current-buffer))))
    (setq truncate-lines (not current-val))
    (message "Truncate long lines ~a"
             (if (not current-val) "enabled" "disabled"))))

(keymap-global-set "C-x x t" toggle-truncate-lines)

;; TODO Support this!
(defvar-local lerp-scroll #f
  "#t means lerp when scrolling.")

(define (toggle-lerp-scroll)
  "Toggle lerping of scroll for the current buffer."
  (let ((current-val (buffer-local-value 'lerp-scroll (current-buffer))))
    (setq lerp-scroll (not current-val))
    (message "Lerp scroll ~a"
             (if (not current-val) "enabled" "disabled"))))

(keymap-global-set "C-x x l" toggle-lerp-scroll)



(defvar-local mode-line-format " -:%+%*-  %b       %p   L%l"
  "TODO.")


(define text-mode-map (make-sparse-keymap))

(define (text-test-1)
  (message "Text mode binding 1"))

(define (text-test-2)
  (message "Text mode binding 2"))

;; TODO OBSIDIAN Those keybinds make C-c p not work anymore
(define-key text-mode-map "C-c 1" text-test-1)
(define-key text-mode-map "C-c 2" text-test-2)

(define-derived-mode text-mode #f "Text"
  "Major mode for editing text."
  (use-local-map text-mode-map)
  (setq-local fill-column 80)
  (message "Text mode enabled"))


(define (test-faces interval-length)
  "Apply error and warning faces alternately across the entire buffer.
INTERVAL-LENGTH specifies how many characters each face segment should be."

  (let ((buffer-size (buffer-size)))
    (let loop ((start-pos 0)
               (use-error? #t))
      (when (< start-pos buffer-size)
        (let ((end-pos (min (+ start-pos interval-length) buffer-size)))
          (put-text-property start-pos
                           end-pos
                           'face
                           (if use-error? 'error 'warning))
          (loop end-pos (not use-error?)))))))


;; (load-theme 'modus-vivendi)
(scheme-mode)


(set-face-attribute 'default nil
                   :family "JetBrains Mono Nerd Font"
                   :weight 'medium
                   :height 170)

(load-theme 'modus-vivendi-tritanopia)
(setq frame-resize-pixelwise t)
;; (load-theme 'dark-one)
;; (rainbow-delimiters-mode)






(defvar eval-expression-print-maximum-character 127
  "The largest integer that will be displayed as a character.
This affects printing by `eval-expression' (via
`eval-expression-print-format').")



;; IT WORKS!
(deftheme 'test-theme "A theme created with the sole scope of testing the theme layering system")

(custom-theme-set-faces 'test-theme
  '(isearch                                   ((t (:foreground "#000000" :background "#44df44"))))
)
