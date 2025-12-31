;;; elisp->scheme.scm --- elisp to scheme transpiler

;;; Commentary:

;; This is experimental and probably won't be needed.
;; (elisp->scheme '(defun double (x) (* x x)))
;; => (define (double x) (* x x))

;;; Code:

;;; Helper predicates

(define (special-form? sym)
  "Check if symbol is a special form that needs transformation."
  (memq sym '(defun defvar defcustom defmacro
              progn prog1 prog2
              if when unless cond
              let let*
              while dolist dotimes
              lambda
              interactive
              quote function)))

;;; Core transformation functions

(define (transform-defun expr)
  "Transform (defun name (args) body...) to (define (name args) body...)"
  (if (and (pair? expr) (eq? (car expr) 'defun))
      (let* ((name (cadr expr))
             (args (caddr expr))
             (body (cdddr expr)))
        `(define (,name ,@args) ,@(map elisp->scheme body)))
      expr))

(define (transform-defvar expr)
  "Transform (defvar name value) to (define name value)"
  (if (and (pair? expr) (eq? (car expr) 'defvar))
      (let ((rest (cdr expr)))
        (cond
         ((= (length rest) 1)
          `(define ,(car rest) #f))
         ((= (length rest) 2)
          `(define ,(car rest) ,(elisp->scheme (cadr rest))))
         ((>= (length rest) 3)
          ;; Ignore docstring
          `(define ,(car rest) ,(elisp->scheme (cadr rest))))
         (else expr)))
      expr))

(define (transform-defcustom expr)
  "Transform (defcustom name value docstring ...) to (define name value)"
  (if (and (pair? expr) (eq? (car expr) 'defcustom))
      (let ((rest (cdr expr)))
        (if (>= (length rest) 2)
            `(define ,(car rest) ,(elisp->scheme (cadr rest)))
            expr))
      expr))

(define (transform-defmacro expr)
  "Transform (defmacro name (args) body...) to define-syntax"
  (if (and (pair? expr) (eq? (car expr) 'defmacro))
      (let* ((name (cadr expr))
             (args (caddr expr))
             (body (cdddr expr)))
        ;; Simplified: just make it a regular function for now
        `(define (,name ,@args) ,@(map elisp->scheme body)))
      expr))

(define (transform-progn expr)
  "Transform (progn body...) to (begin body...)"
  (if (and (pair? expr) (eq? (car expr) 'progn))
      `(begin ,@(map elisp->scheme (cdr expr)))
      expr))

(define (transform-prog1 expr)
  "Transform (prog1 first rest...) to (let ((result first)) rest... result)"
  (if (and (pair? expr) (eq? (car expr) 'prog1))
      (let* ((first (cadr expr))
             (rest (cddr expr))
             (result-var (gensym "result")))
        `(let ((,result-var ,(elisp->scheme first)))
           ,@(map elisp->scheme rest)
           ,result-var))
      expr))

(define (transform-if expr)
  "Transform if expressions (mostly same in both)"
  (if (and (pair? expr) (eq? (car expr) 'if))
      (let* ((test (cadr expr))
             (then-clause (caddr expr))
             (else-clause (if (>= (length expr) 4) (cadddr expr) #f)))
        (if else-clause
            `(if ,(elisp->scheme test)
                 ,(elisp->scheme then-clause)
                 ,(elisp->scheme else-clause))
            `(if ,(elisp->scheme test)
                 ,(elisp->scheme then-clause))))
      expr))

(define (transform-when expr)
  "Transform (when test body...) to (when test body...)"
  (if (and (pair? expr) (eq? (car expr) 'when))
      (let ((test (cadr expr))
            (body (cddr expr)))
        `(when ,(elisp->scheme test)
           ,@(map elisp->scheme body)))
      expr))

(define (transform-unless expr)
  "Transform (unless test body...) to (unless test body...)"
  (if (and (pair? expr) (eq? (car expr) 'unless))
      (let ((test (cadr expr))
            (body (cddr expr)))
        `(unless ,(elisp->scheme test)
           ,@(map elisp->scheme body)))
      expr))

(define (transform-cond expr)
  "Transform cond expressions (same in both)"
  (if (and (pair? expr) (eq? (car expr) 'cond))
      (let ((clauses (cdr expr)))
        `(cond ,@(map (lambda (clause)
                        (if (pair? clause)
                            (cons (elisp->scheme (car clause))
                                  (map elisp->scheme (cdr clause)))
                            clause))
                      clauses)))
      expr))

(define (transform-let expr)
  "Transform let expressions (same in both)"
  (if (and (pair? expr) (eq? (car expr) 'let))
      (let ((bindings (cadr expr))
            (body (cddr expr)))
        `(let ,(map (lambda (binding)
                      (if (pair? binding)
                          `(,(car binding) ,(elisp->scheme (cadr binding)))
                          binding))
                    bindings)
           ,@(map elisp->scheme body)))
      expr))

(define (transform-let* expr)
  "Transform let* expressions (same in both)"
  (if (and (pair? expr) (eq? (car expr) 'let*))
      (let ((bindings (cadr expr))
            (body (cddr expr)))
        `(let* ,(map (lambda (binding)
                       (if (pair? binding)
                           `(,(car binding) ,(elisp->scheme (cadr binding)))
                           binding))
                     bindings)
           ,@(map elisp->scheme body)))
      expr))

(define (transform-while expr)
  "Transform (while test body...) to a while loop"
  (if (and (pair? expr) (eq? (car expr) 'while))
      (let ((test (cadr expr))
            (body (cddr expr)))
        `(while ,(elisp->scheme test)
           ,@(map elisp->scheme body)))
      expr))

(define (transform-dolist expr)
  "Transform (dolist (var list) body...) to (for-each (lambda (var) body...) list)"
  (if (and (pair? expr) (eq? (car expr) 'dolist))
      (let* ((spec (cadr expr))
             (var (car spec))
             (list-expr (cadr spec))
             (body (cddr expr)))
        `(for-each (lambda (,var) ,@(map elisp->scheme body))
                   ,(elisp->scheme list-expr)))
      expr))

(define (transform-lambda expr)
  "Transform lambda expressions (same in both)"
  (if (and (pair? expr) (eq? (car expr) 'lambda))
      (let ((args (cadr expr))
            (body (cddr expr)))
        `(lambda ,args ,@(map elisp->scheme body)))
      expr))

(define (transform-quote expr)
  "Transform quoted expressions"
  (if (and (pair? expr) (eq? (car expr) 'quote))
      `(quote ,(transform-quoted (cadr expr)))
      expr))

(define (transform-quoted x)
  "Transform symbols inside quoted expressions (nil -> #f, t -> #t)"
  (cond
   ((eq? x 'nil) #f)
   ((eq? x 't) #t)
   ((pair? x) (cons (transform-quoted (car x))
                    (transform-quoted (cdr x))))
   (else x)))

(define (transform-function expr)
  "Transform (function ...) - in elisp this is like quote for functions"
  (if (and (pair? expr) (eq? (car expr) 'function))
      (elisp->scheme (cadr expr))
      expr))

(define (transform-interactive expr)
  "Transform interactive forms - for now, just ignore them"
  (if (and (pair? expr) (eq? (car expr) 'interactive))
      `(begin) ; No-op
      expr))

;;; Symbol transformations

(define (transform-symbol sym)
  "Transform individual symbols (nil -> #f, t -> #t)"
  (cond
   ((eq? sym 'nil) #f)
   ((eq? sym 't) #t)
   (else sym)))

;;; Main transformation dispatch

(define (elisp->scheme expr)
  "Convert an elisp expression to scheme."
  (cond
   ;; Atoms
   ((symbol? expr)
    (transform-symbol expr))
   
   ((not (pair? expr))
    expr)
   
   ;; Special forms
   ((and (pair? expr) (symbol? (car expr)))
    (let ((head (car expr)))
      (case head
        ((defun)       (transform-defun expr))
        ((defvar)      (transform-defvar expr))
        ((defcustom)   (transform-defcustom expr))
        ((defmacro)    (transform-defmacro expr))
        ((progn)       (transform-progn expr))
        ((prog1)       (transform-prog1 expr))
        ((if)          (transform-if expr))
        ((when)        (transform-when expr))
        ((unless)      (transform-unless expr))
        ((cond)        (transform-cond expr))
        ((let)         (transform-let expr))
        ((let*)        (transform-let* expr))
        ((while)       (transform-while expr))
        ((dolist)      (transform-dolist expr))
        ((lambda)      (transform-lambda expr))
        ((quote)       (transform-quote expr))
        ((function)    (transform-function expr))
        ((interactive) (transform-interactive expr))
        (else
         ;; Regular function call - recursively transform arguments
         (map elisp->scheme expr)))))
   
   ;; Default: recursively transform
   (else
    (map elisp->scheme expr))))

(define (elisp->scheme-string str)
  "Read elisp string and convert to scheme string."
  (call-with-input-string str
    (lambda (port)
      (let ((expr (read port)))
        (with-output-to-string
          (lambda ()
            (write (elisp->scheme expr))))))))

;;; Tests

;; (elisp->scheme '(defun double (x) (* x x)))
;; => (define (double x) (* x x))

;; (elisp->scheme '(defvar my-var 42))
;; => (define my-var 42)

;; (elisp->scheme '(setq x 10))
;; => (setq x 10)

;; (elisp->scheme '(if (> x 0) (print "positive") (print "negative")))
;; => (if (> x 0) (print "positive") (print "negative"))

;; (elisp->scheme '(progn (setq x 1) (setq y 2) (+ x y)))
;; => (begin (setq x 1) (setq y 2) (+ x y))

;; (elisp->scheme '(let ((x 1) (y 2)) (+ x y)))
;; => (let ((x 1) (y 2)) (+ x y))

;; (elisp->scheme '(dolist (item '(1 2 3)) (print item)))
;; => (for-each (lambda (item) (print item)) '(1 2 3))

;; (elisp->scheme 'nil)
;; => #f

;; (elisp->scheme 't)
;; => #t

;; (elisp->scheme '(quote (a nil t b)))
;; => '(a #f #t b)
