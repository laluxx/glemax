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
