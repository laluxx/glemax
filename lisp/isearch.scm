;;; isearch.scm --- Incremental search minor mode

;; Author: Laluxx
;; Maintainer: Laluxx
;; Keywords: matching
;; Package: glemax

;; This file is part of Glemax.

;;; TODO [0/2]
;; - [ ] Optimize this, it's horrible with big buffers.
;; - [ ] Slowly add all options that isearch has.

;;; Code:

;;; State


(defvar-local isearch-mode nil "Non-nil when isearch-mode is active in this buffer.")
(defvar isearch-string "" "The current incremental search string.")
(defvar isearch-forward? t "Non-nil means searching forward.")
(defvar isearch-origin 0 "Buffer position where the search started.")
(defvar isearch-current-match nil "Start position of current match, or nil.")
(defvar isearch-success? t "Non-nil means the last search succeeded.")
(defvar isearch-wrapped? nil "Non-nil means the search has wrapped around.")
(defvar isearch-wrap-count 0 "Number of times search has wrapped.")
(defvar isearch-overlay-start nil "Start of current match highlight.")
(defvar isearch-overlay-end nil "End of current match highlight.")
(defvar isearch-match-length 0 "Length of the last successful match prefix.")

(defvar isearch-lazy-highlight-overlays '()
 "List of (start . end) ranges highlighted with lazy-highlight.")

(defvar isearch-wrap-pause t
  "If non-nil, pause at buffer boundary with Failing before wrapping.")

(defvar isearch-wrap-stop nil
  "If non-nil, stop completely on pause once already overwrapped.")

;;; Prompt

(defun isearch-prompt-string ()
  "Return echo-area prompt reflecting current search state."
  (cond
   ((and (not isearch-success?) isearch-wrapped?)
    (if isearch-forward? "Failing overwrapped I-search: " "Failing overwrapped I-search backward: "))
   ((not isearch-success?)
    (if isearch-forward? "Failing I-search: " "Failing I-search backward: "))
   (isearch-wrapped?
    (if isearch-forward? "Overwrapped I-search: " "Overwrapped I-search backward: "))
   (else
    (if isearch-forward? "I-search: " "I-search backward: "))))

(defun isearch-update-message ()
  "Display the isearch prompt with faces in the echo area."
  (let* ((prompt   (isearch-prompt-string))
         (plen     (string-length prompt))
         (slen     (string-length isearch-string))
         (good-end isearch-match-length)
         (cur      (current-buffer))
         (mb       (window-buffer (minibuffer-window))))
    (message (string-append prompt isearch-string))
    (set-buffer mb)
    (put-text-property 0 plen 'face 'minibuffer-prompt)
    ;; Only mark the failing tail, no face on the matched portion
    (when (< good-end slen)
      (put-text-property (+ plen good-end) (+ plen slen) 'face 'isearch-fail))
    (set-buffer cur)))

;;; Search

(defun isearch-get-text ()
  "Return the entire buffer contents as a string."
  (buffer-substring 0 (buffer-size)))

(defun isearch-find-forward (str from-pos)
  "Search forward for STR starting at FROM-POS. Returns match start or nil."
  (if (string=? str "")
      from-pos
      (let* ((text (isearch-get-text))
             (tlen (string-length text))
             (slen (string-length str)))
        (let loop ((pos from-pos))
          (cond
           ((> (+ pos slen) tlen) nil)
           ((string=? (substring text pos (+ pos slen)) str) pos)
           (else (loop (+ pos 1))))))))

(defun isearch-find-backward (str from-pos)
  "Search backward for STR starting at FROM-POS. Returns match start or nil."
  (if (string=? str "")
      from-pos
      (let* ((text (isearch-get-text))
             (tlen (string-length text))
             (slen (string-length str))
             (start (min from-pos (- tlen slen))))
        (let loop ((pos start))
          (cond
           ((< pos 0) nil)
           ((string=? (substring text pos (+ pos slen)) str) pos)
           (else (loop (- pos 1))))))))

(defun isearch-longest-match-prefix (str)
  "Return the length of the longest prefix of STR that exists anywhere in the buffer."
  (let loop ((len (string-length str)))
    (cond
     ((= len 0) 0)
     ((let ((sub (substring str 0 len)))
        (or (isearch-find-forward sub 0)
            (isearch-find-backward sub (buffer-size))))
      len)
     (else (loop (- len 1))))))

(defun isearch-apply-match (match-pos)
  "Move point to match and highlight it."
  (let ((end-pos (+ match-pos (string-length isearch-string))))
    (setq isearch-current-match match-pos)
    (goto-char (if isearch-forward? end-pos match-pos))
    (isearch-highlight match-pos end-pos)))

(defun isearch-try-wrap ()
  "Wrap search once; on second wrap signal failing-overwrapped."
  (let* ((wrap-from (if isearch-forward? 0 (- (buffer-size) 1)))
         (match     (if isearch-forward?
                        (isearch-find-forward  isearch-string wrap-from)
                        (isearch-find-backward isearch-string wrap-from))))
    (if match
        (begin
          (setq isearch-wrap-count (+ isearch-wrap-count 1))
          (setq isearch-success?  t)
          (setq isearch-wrapped?  t)
          (isearch-apply-match match)
          t)
        (begin
          (setq isearch-success? nil)
          (setq isearch-current-match nil)
          nil))))

(defun isearch-update ()
  "Re-run search from origin; update point, highlight, and echo area."
  (isearch-clear-highlight)
  (if (string=? isearch-string "")
      (begin
        (setq isearch-current-match nil)
        (setq isearch-success?      t)
        (setq isearch-wrapped?      nil)
        (setq isearch-wrap-count    0)
        (setq isearch-match-length  0)
        (goto-char isearch-origin))
      (let ((match (if isearch-forward?
                       (isearch-find-forward  isearch-string isearch-origin)
                       (isearch-find-backward isearch-string isearch-origin))))
        (if match
            (begin
              (setq isearch-success?     t)
              (setq isearch-wrapped?     nil)
              (setq isearch-wrap-count   0)
              (setq isearch-match-length (string-length isearch-string))
              (isearch-apply-match match))
            (begin
              (setq isearch-success?     nil)
              (setq isearch-match-length (isearch-longest-match-prefix isearch-string))
              (isearch-try-wrap)))))
  (isearch-update-message))

(defun isearch-repeat (forward?)
  "Advance to the next/previous occurrence."
  (when (not (string=? isearch-string ""))
    (let* ((cur          isearch-current-match)
           (slen         (string-length isearch-string))
           (advance-from (if forward?
                             (if cur (+ cur 1) isearch-origin)
                             (if cur (- cur 1) isearch-origin)))
           (match        (if forward?
                             (isearch-find-forward  isearch-string advance-from)
                             (isearch-find-backward isearch-string advance-from))))
      (cond
        ;; Found a match — move to it
        (match
         (setq isearch-success?     t)
         (setq isearch-match-length slen)
         (isearch-apply-match match))
        ;; No match, pause enabled, still succeeding — first wall hit, go Failing
        ((and isearch-wrap-pause isearch-success?)
         (setq isearch-success?     nil)
         (setq isearch-match-length 0))
        ;; Already wrapped, failing, and stop enabled — stay put
        ((and isearch-wrap-stop isearch-wrapped? (not isearch-success?))
         (setq isearch-success?     nil)
         (setq isearch-match-length 0))
        ;; Otherwise wrap — covers: pause off, or pause already triggered, or wrap-stop off
        (else
         (let* ((wrap-from (if forward? 0 (- (buffer-size) 1)))
                (wmatch    (if forward?
                               (isearch-find-forward  isearch-string wrap-from)
                               (isearch-find-backward isearch-string wrap-from))))
           (if wmatch
               (begin
                 (setq isearch-success?     t)
                 (setq isearch-wrapped?     t)
                 (setq isearch-match-length slen)
                 (isearch-apply-match wmatch))
               (begin
                 (setq isearch-success?     nil)
                 (setq isearch-match-length 0)))))))
    (isearch-update-message)))

;;; Highlight


(defun isearch-highlight (start end)
  "Highlight current match with isearch face and all others with lazy-highlight."
  (isearch-clear-highlight)
  (when (< start end)
    ;; Highlight all occurrences with lazy-highlight first
    (let* ((text (isearch-get-text))
           (tlen (string-length text))
           (slen (string-length isearch-string)))
      (when (> slen 0)
        (let loop ((pos 0))
          (when (<= (+ pos slen) tlen)
            (when (string=? (substring text pos (+ pos slen)) isearch-string)
              (unless (= pos start)
                (put-text-property pos (+ pos slen) 'face 'lazy-highlight)
                (setq isearch-lazy-highlight-overlays
                      (cons (cons pos (+ pos slen))
                            isearch-lazy-highlight-overlays))))
            (loop (+ pos 1))))))
    ;; Highlight current match on top
    (put-text-property start end 'face 'isearch)
    (setq isearch-overlay-start start)
    (setq isearch-overlay-end   end)))

(defun isearch-clear-highlight ()
  "Remove current match highlight and all lazy-highlight overlays."
  ;; Clear lazy highlights
  (for-each (lambda (range)
              (remove-text-properties (car range) (cdr range) '()))
            isearch-lazy-highlight-overlays)
  (setq isearch-lazy-highlight-overlays '())
  ;; Clear current match
  (when (and isearch-overlay-start isearch-overlay-end)
    (remove-text-properties isearch-overlay-start isearch-overlay-end '())
    (setq isearch-overlay-start nil)
    (setq isearch-overlay-end   nil)))

;;; Entry / exit

(defun isearch-mode (arg)
  "Minor mode for incremental search."
  (minor-mode-toggle 'isearch-mode arg nil))

(defun isearch-start (forward?)
  "Initialise state and activate isearch-mode."
  (setq isearch-string        "")
  (setq isearch-forward?      forward?)
  (setq isearch-origin        (point))
  (setq isearch-current-match nil)
  (setq isearch-success?      t)
  (setq isearch-wrapped?      nil)
  (setq isearch-wrap-count    0)
  (setq isearch-match-length  0)
  (isearch-mode 1)
  (isearch-update-message))

(defun isearch-exit ()
  "Exit isearch, leaving point at the current match."
  (isearch-clear-highlight)
  (message "")
  (isearch-mode -1))

(defun isearch-abort ()
  "Abort isearch and restore point to origin."
  (isearch-clear-highlight)
  (goto-char isearch-origin)
  (message "Quit")
  (isearch-mode -1))

;;; Commands

(defun isearch-printing-char ()
  "Append this-command-event to the search string."
  (let* ((event this-command-event)
         (char  (cond
                 ((char? event)    event)
                 ((integer? event) (integer->char event))
                 (else nil))))
    (when char
      (setq isearch-string (string-append isearch-string (string char)))
      ;; Typing resets wrap state — search forward from origin fresh
      (setq isearch-wrapped?  nil)
      (setq isearch-wrap-count 0)
      (isearch-update))))

(defun isearch-delete-char ()
  "Remove the last character from the search string."
  (when (> (string-length isearch-string) 0)
    (setq isearch-string
          (substring isearch-string 0 (- (string-length isearch-string) 1)))
    (setq isearch-wrapped?   nil)
    (setq isearch-wrap-count 0)
    (isearch-update)))

(defun isearch-repeat-forward ()
  "Repeat search forward, or if going backward flip to forward from current pos."
  (if isearch-forward?
      (isearch-repeat t)
      (begin
        ;; Flip direction: search forward from wherever point is now
        (setq isearch-forward?   t)
        (setq isearch-wrapped?   nil)
        (setq isearch-wrap-count 0)
        (setq isearch-origin
              (if isearch-current-match
                  isearch-current-match
                  (point)))
        (isearch-update))))

(defun isearch-repeat-backward ()
  "Repeat search backward, or if going forward flip to backward from current pos."
  (if (not isearch-forward?)
      (isearch-repeat nil)
      (begin
        ;; Flip direction: search backward from wherever point is now
        (setq isearch-forward?   nil)
        (setq isearch-wrapped?   nil)
        (setq isearch-wrap-count 0)
        (setq isearch-origin
              (if isearch-current-match
                  isearch-current-match
                  (point)))
        (isearch-update))))

;;; Keymap

(define isearch-mode-map (make-sparse-keymap))

(let loop ((code 32))
  (when (<= code 126)
    (define-key isearch-mode-map (string (integer->char code)) isearch-printing-char)
    (loop (+ code 1))))

(let loop ((code 160))
  (when (<= code 255)
    (define-key isearch-mode-map (string (integer->char code)) isearch-printing-char)
    (loop (+ code 1))))

(define-key isearch-mode-map "C-s"         isearch-repeat-forward)
(define-key isearch-mode-map "C-r"         isearch-repeat-backward)
(define-key isearch-mode-map "RET"         isearch-exit)
(define-key isearch-mode-map "C-g"         isearch-abort)
(define-key isearch-mode-map "DEL"         isearch-delete-char)
(define-key isearch-mode-map "BS"          isearch-delete-char)
(define-key isearch-mode-map "<backspace>" isearch-delete-char)
;; (define-key isearch-mode-map "C-h"         isearch-delete-char)

;;; Minor mode registration

(register-minor-mode
 'isearch-mode
 (make-minor-mode-info
  " Isearch"
  isearch-mode-map
  nil
  nil
  nil))

;;; Global entry points

(defun isearch-forward ()
  "Start incremental search forward."
  (isearch-start t))

(defun isearch-backward ()
  "Start incremental search backward."
  (isearch-start nil))

(define-key (current-global-map) "C-s" isearch-forward)
(define-key (current-global-map) "C-r" isearch-backward)
