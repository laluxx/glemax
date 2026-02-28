;;; rainbow-delimiters.scm --- Highlight delimiters by depth

;;; Commentary:

;; Highlights parentheses, brackets, and braces according to nesting depth.
;; Hooks into post-self-insert-hook to run after tree-sitter highlighting.
;; Uses integer face IDs to match how treesit applies faces.
;; Efficiently skips strings/comments by jumping over property intervals.

;;; Code:

(define rainbow-delimiters-max-face-count 9)
(define rainbow-delimiters--enabled nil)

(define rainbow-delimiters--depth-ids nil)
(define rainbow-delimiters--unmatched-id nil)
(define rainbow-delimiters--mismatched-id nil)
(define rainbow-delimiters--skip-ids nil)

(define (rainbow-delimiters--init-face-ids)
  "Cache all face IDs. Called once when mode is first enabled."
  (unless rainbow-delimiters--depth-ids
    (setq rainbow-delimiters--depth-ids
          (list->vector
           (map (lambda (i)
                  (face-id-from-name
                   (string-append "rainbow-delimiters-depth-"
                                  (number->string i)
                                  "-face")))
                '(1 2 3 4 5 6 7 8 9))))
    (setq rainbow-delimiters--unmatched-id
          (face-id-from-name "rainbow-delimiters-unmatched-face"))
    (setq rainbow-delimiters--mismatched-id
          (face-id-from-name "rainbow-delimiters-mismatched-face"))
    (setq rainbow-delimiters--skip-ids
          (list->vector
           (map face-id-from-name
                '("font-lock-string-face"
                  "font-lock-comment-face"
                  "font-lock-comment-delimiter-face"
                  "font-lock-doc-face"
                  "font-lock-regexp-face"))))))

(define (rainbow-delimiters--skip-face? face-id)
  "Return t if FACE-ID is a string or comment face to skip."
  (let ((len (vector-length rainbow-delimiters--skip-ids)))
    (let loop ((i 0))
      (cond
       ((>= i len) nil)
       ((= face-id (vector-ref rainbow-delimiters--skip-ids i)) t)
       (else (loop (+ i 1)))))))

(define (rainbow-delimiters--depth-face-id depth)
  "Return integer face ID for DEPTH."
  (if (<= depth 0)
      rainbow-delimiters--unmatched-id
      (vector-ref rainbow-delimiters--depth-ids
                  (modulo (- depth 1) rainbow-delimiters-max-face-count))))

(define (rainbow-delimiters--opening? code)
  (or (= code 40)    ; (
      (= code 91)    ; [
      (= code 123))) ; {

(define (rainbow-delimiters--closing? code)
  (or (= code 41)    ; )
      (= code 93)    ; ]
      (= code 125))) ; }

(define (rainbow-delimiters--matching? open close)
  (or (and (= open 40)  (= close 41))
      (and (= open 91)  (= close 93))
      (and (= open 123) (= close 125))))

(define (rainbow-delimiters-apply)
  "Apply rainbow delimiter faces to the current buffer.
Efficiently skips string/comment spans using property interval jumps."
  (let ((size (buffer-size)))
    (let loop ((pos   0)
               (depth 0)
               (stack '()))
      (when (< pos size)
        ;; Check the face of the current interval
        (let* ((face (get-text-property pos 'face))
               (skip (and (number? face)
                          (rainbow-delimiters--skip-face? face))))
          (if skip
              ;; Jump to end of this string/comment interval in one step
              (let ((next (next-property-change pos)))
                (loop next depth stack))
              ;; Normal character — check if delimiter
              (let* ((ch   (char-after pos))
                     (code (cond ((not ch)   nil)
                                 ((char? ch) (char->integer ch))
                                 (else       ch))))
                (cond
                 ((not code)
                  (loop (+ pos 1) depth stack))

                 ((rainbow-delimiters--opening? code)
                  (let ((new-depth (+ depth 1)))
                    (put-text-property pos (+ pos 1)
                                       'face
                                       (rainbow-delimiters--depth-face-id new-depth))
                    (loop (+ pos 1)
                          new-depth
                          (cons (cons new-depth code) stack))))

                 ((rainbow-delimiters--closing? code)
                  (if (null? stack)
                      (begin
                        (put-text-property pos (+ pos 1)
                                           'face
                                           rainbow-delimiters--unmatched-id)
                        (loop (+ pos 1) depth stack))
                      (let* ((top        (car stack))
                             (open-char  (cdr top))
                             (open-depth (car top)))
                        (put-text-property pos (+ pos 1)
                                           'face
                                           (if (rainbow-delimiters--matching? open-char code)
                                               (rainbow-delimiters--depth-face-id open-depth)
                                               rainbow-delimiters--mismatched-id))
                        (loop (+ pos 1)
                              (max 0 (- depth 1))
                              (cdr stack)))))

                 (else
                  (loop (+ pos 1) depth stack))))))))))

(define (rainbow-delimiters-mode . args)
  "Toggle Rainbow Delimiters mode.
With positive numeric argument, enable.
With 0 or negative argument, disable.
With no argument, toggle."
  (let* ((arg       (if (null? args) 'toggle (car args)))
         (new-value (cond
                     ((eq? arg 'toggle) (not rainbow-delimiters--enabled))
                     ((number? arg)     (> arg 0))
                     (else              (if arg t nil)))))
    (unless (eq? rainbow-delimiters--enabled new-value)
      (setq rainbow-delimiters--enabled new-value)
      (if rainbow-delimiters--enabled
          (begin
            (rainbow-delimiters--init-face-ids)
            (add-hook 'post-self-insert-hook rainbow-delimiters-apply)
            (rainbow-delimiters-apply)
            (message "Rainbow-Delimiters mode enabled in current buffer"))
          (begin
            (remove-hook 'post-self-insert-hook rainbow-delimiters-apply)
            (message "Rainbow-Delimiters mode disabled in current buffer"))))
    rainbow-delimiters--enabled))

(define (rainbow-delimiters-mode-enable)
  "Enable Rainbow Delimiters mode."
  (rainbow-delimiters-mode 1))

(define (rainbow-delimiters-mode-disable)
  "Disable Rainbow Delimiters mode."
  (rainbow-delimiters-mode 0))
