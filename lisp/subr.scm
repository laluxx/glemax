;;; subr.scm --- basic lisp subroutines for Glemax


;;; Commentary:

;; Core Lisp functionality that has to be loaded very early

;;; Code:

;; TODO call the function automatically
;; when we setq `frame-resize-pixelwise'
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
