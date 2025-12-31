;;; derived.scm --- allow inheritance of major modes


;;; Commentary:

;; Glemax is already, in a sense, object oriented -- each object
;; (buffer) belongs to a class (major mode), and that class defines
;; the relationship between messages (input events) and methods
;; (commands) by means of a keymap.
;;
;; The only thing missing is a good scheme of inheritance.
;; -- but generally, each major mode ends up reinventing the wheel.
;; Ideally, someone should redesign all of Emacs's major modes to
;; follow a more conventional object-oriented system: when defining a
;; new major mode, the user should need only to name the existing mode
;; it is most similar to, then list the (few) differences.
;;
;; In the mean time, this package offers most of the advantages of
;; full inheritance with the existing major modes.  The macro
;; `define-derived-mode' allows the user to make a variant of an existing
;; major mode, with its own keymap.  The new mode will inherit the key
;; bindings of its parent, and will, in fact, run its parent first
;; every time it is called.

;;; Code:

(defvar-local major-mode 'fundamental-mode
  "Symbol for current buffer's major mode.
The default value (normally `fundamental-mode') affects new buffers.
A value of nil means to use the current buffer's major mode, provided
it is not marked as \"special\".")


;; Symbol property storage for mode relationships
(define mode-plist (make-hash-table))

(define (put symbol prop value)
  "Store VALUE in SYMBOL's property list under indicator PROP."
  (let ((plist (hash-ref mode-plist symbol '())))
    (hash-set! mode-plist symbol (acons prop value plist))))

(define (get symbol prop)
  "Return the value of SYMBOL's PROP property."
  (let ((plist (hash-ref mode-plist symbol '())))
    (assq-ref plist prop)))

(define (derived-mode? . modes)
  "Return #t if the current major mode is derived from one of MODES.
MODES should be a list of symbols or a single mode symbol instead of a list.
This examines the parent modes set by `define-derived-mode' and also
TODO additional ones set by `derived-mode-add-parents'."
  (let loop ((current major-mode))
    (cond
     ((memq current modes) #t)
     ((not current) #f)
     (else
      (let ((parent (get current 'derived-mode-parent)))
        (if parent
            (loop parent)
            #f))))))

(define-syntax define-derived-mode
  (syntax-rules ()
    ((_ mode parent-mode name docstring . body)
     (begin
       ;; Store the parent mode relationship
       (put 'mode 'derived-mode-parent 'parent-mode)
       
       (define (mode)
         docstring
         (kill-all-local-variables)
         (when parent-mode (parent-mode))
         (setq major-mode 'mode)
         (setq mode-name name)
         . body)))))
