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


(define-syntax define-derived-mode
  (syntax-rules ()
    ((_ mode parent-mode name docstring . body)
     (define (mode)
       docstring
       (kill-all-local-variables)
       (when parent-mode (parent-mode))
       (setq 'major-mode 'mode)
       (setq 'mode-name name)
       . body))))
