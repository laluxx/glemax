;;; subr.scm --- basic lisp subroutines for Glemax


;;; Commentary:

;; Core Lisp functionality that has to be loaded very early

;;; Code:

(define nil #f)
(define t #t)

(define-syntax set!
  (syntax-rules ()
    ((_ . args)
     (scm-error 'misc-error
                #f
                "Use setq, not set!"
                '()
                '()))))

(define (set-var-doc! v doc)
  (set-object-property! v 'documentation doc))

(define (variable-documentation var)
  "variable-documentation var
Return the documentation property associated with `var'."
  (object-property var 'documentation))

(define-syntax setq
  (syntax-rules ()
    ((setq var val)
     (setq-impl 'var val))))

(define-syntax setq-local
  (syntax-rules ()
    ((setq-local var val)
     (begin
       (when (not (local-variable? 'var))
         (make-local-variable 'var))
       (set 'var val)))))

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

(define-syntax defun
  (syntax-rules (&rest &optional)
    ;; No special args
    ((_ name (args ...) body ...)
     (define (name args ...)
       body ...))

    ;; With &rest: (defun name (arg1 arg2 &rest rest-args) body ...)
    ((_ name (args ... &rest rest-arg) body ...)
     (define (name args ... . rest-arg)
       body ...))

    ;; TODO: &optional is more complex, would need a different approach
    ))

(define-syntax defvar
  (syntax-rules ()
    ((_ name)
     (define name #f))
    ((_ name value)
     (define name value))
    ((_ name value docstring)
     (define name value))))

(define-syntax progn
  (syntax-rules ()
    ((_ body ...)
     (begin body ...))))


;;; Faces

(define-syntax defface
  (syntax-rules ()
    ((_ name spec)
     (%defface 'name spec))
    ((_ name spec doc)
     (%defface 'name spec doc))))

;;; Hooks

(define (add-hook hook function)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.

HOOK should be a symbol.  If HOOK is void, or if HOOK's value is a
single function, it is changed to a list of functions.

The place where the function is added depends on the DEPTH
parameter.  DEPTH defaults to 0.  By convention, it should be
a number between -100 and 100 where 100 means that the function
should be at the very end of the list, whereas -100 means that
the function should always come first.
Since nothing is \"always\" true, don't use 100 nor -100.
When two functions have the same depth, the new one gets added after the
old one if depth is strictly positive and before otherwise.

For backward compatibility reasons, a symbol other than nil is
interpreted as a DEPTH of 90.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its global value.
This makes the hook buffer-local, and it makes t a member of the
buffer-local value.  That acts as a flag to run the hook
functions of the global value as well as in the local value.

FUNCTION may be any valid function, but it's recommended to use a
function symbol and not a lambda form.  Using a symbol will
ensure that the function is not re-added if the function is
edited, and using lambda forms may also have a negative
performance impact when running `add-hook' and `remove-hook'."
  (let ((var (module-variable (current-module) hook)))
    (when var
      (let ((hook-val (variable-ref var)))
        (unless (memq function hook-val)
          (module-define! (current-module) hook
                         (cons function hook-val)))))))

(define (remove-hook hook function)
  "Remove FUNCTION from HOOK's functions.

HOOK should be a symbol, and FUNCTION may be any valid function.
Do nothing if HOOK does not currently contain FUNCTION.
Compare functions with `equal`, which means that it can be
slow if FUNCTION is not a symbol.  See add-hook.

The optional third argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.

Interactively, prompt for the various arguments (skipping local
unless HOOK has both local and global functions).  If multiple
functions have the same representation under princ, the first
one will be removed."
  (let ((var (module-variable (current-module) hook)))
    (when var
      (let ((hook-val (variable-ref var)))
        (module-define! (current-module) hook
                       (delq function hook-val))))))
