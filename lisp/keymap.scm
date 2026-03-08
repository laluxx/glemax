;;; keymap.scm --- Keymap functions

;; Maintainer: Laluxx
;; Keywords: internal
;; Package: glemax

;; This file is part of Glemax.

;;; Commentary:

;; This library deals with the keymap binding interface: The
;; only key syntax allowed by these functions is the `kbd' one.

;;; Code:


(define (key-valid? keys)
  (define (strip-modifiers k)
    ;; Modifiers must appear in exactly this order: A- C- H- M- S- s-
    ;; We match the whole prefix in one go, not greedily one at a time
    (let ((m (string-match "^(A-)?(C-)?(H-)?(M-)?(S-)?(s-)?(.*)$" k)))
      (if m
          (match:substring m 7)  ;; everything after the optional modifiers
          k)))

  (define (valid-single-key? k)
    (or
     ;; Single printable ASCII (not control char < space, not meta-range 127-255)
     (and (= (string-length k) 1)
          (let ((c (char->integer (string-ref k 0))))
            (and (>= c (char->integer #\space))
                 (not (<= 127 c 255)))))
     ;; Angle bracket event: <word-chars> with no modifiers inside
     (and (string-prefix? "<" k)
          (string-suffix? ">" k)
          (let ((inner (substring k 1 (- (string-length k) 1))))
            (and (> (string-length inner) 0)
                 (string-match "^[-_A-Za-z0-9]+$" inner))))
     ;; Special shorthand names
     (member k '("NUL" "RET" "TAB" "LFD" "ESC" "SPC" "DEL"))))

  (and
   (string? keys)
   (> (string-length keys) 0)
   (not (string-prefix? " " keys))
   (not (string-suffix? " " keys))
   (call-with-current-continuation
    (lambda (exit)
      (for-each
       (lambda (key)
         (when (= (string-length key) 0)
           (exit #f))
         ;; Check that modifiers are in strict order by matching the whole
         ;; ordered prefix at once — if stripping yields the same string
         ;; but there's still a modifier-looking prefix, order was wrong
         (let* ((stripped (strip-modifiers key))
                (prefix-len (- (string-length key) (string-length stripped))))
           (when (= (string-length stripped) 0)
             (exit #f))
           ;; If after stripping there's still X- pattern, modifiers were
           ;; out of order (e.g. M-C- would leave C- after stripping M-)
           (when (string-match "^[ACHMSs]-" stripped)
             (exit #f))
           (unless (valid-single-key? stripped)
             (exit #f))))
       (string-split keys #\space))
      #t))))
