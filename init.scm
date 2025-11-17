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
(keychord-bind "S-<backspace>" delete-region)
(keychord-bind "C-w" kill-region)
(keychord-bind "C-y" yank)
(keychord-bind "C-k" kill-line)
(keychord-bind "C-x C-x" exchange-point-and-mark)
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

(define blink-cursor-mode #t)
(define blink-cursor-blinks 10)
(define blink-cursor-interval 0.5)
(define blink-cursor-delay 0.5)
(define visible-mark-mode #t)
(define electric-pair-mode #t)
(define kill-whole-line #t)


(define (point-min) 0)

(define (point-max)
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
  "Delete all spaces and tabs around point."
  (let ((start (point)))
    ;; Delete forward
    (while (and (not (eob?))
                (let ((ch (char-after (point))))
                  (or (= ch 32) (= ch 9)))) ; space or tab
      (delete-char)); TODO This should have arg 1
    ;; Delete backward
    (while (and (not (bob?))
         (let ((ch (char-before (point))))
                  (or (= ch 32) (= ch 9))))
      (delete-backward-char)))); TODO This should have arg 1

(keychord-bind "M-\\" delete-horizontal-space)


(define (chatty-add chatty-name . nums)
        (format #t "<~a> If you add those together you get ~a!\n"
                chatty-name (apply + nums)))
