;;; minor-mode.scm --- Minor mode infrastructure

;;; Commentary:

;; This file implements minor modes.
;; Minor modes are independent, optional features that can be toggled on/off.
;; They can be buffer-local or global, have keymaps, and run hooks.

;; Example buffer-local mode:
;;
;; (define-minor-mode auto-fill-mode
;;   "Toggle automatic line breaking (Auto Fill mode)."
;;   :lighter " Fill"
;;   :keymap auto-fill-mode-map
;;   :enable-hook (lambda () (message "Auto Fill enabled"))
;;   :disable-hook (lambda () (message "Auto Fill disabled"))
;;   (if auto-fill-mode
;;       (set! fill-column 80)
;;       (set! fill-column nil)))

;; Example global mode:
;;
;; (define-minor-mode global-hl-line-mode
;;   "Toggle highlighting of current line globally."
;;   :global #t
;;   :lighter " GHL"
;;   (if global-hl-line-mode
;;       (enable-highlighting)
;;       (disable-highlighting)))

;;; Code:

;; Hash table to store minor mode definitions
(define minor-mode-table (make-hash-table))

;; Structure to hold minor mode metadata
(define (make-minor-mode-info lighter keymap enable-hook disable-hook global?)
  "Create a minor mode info structure."
  (list (cons 'lighter lighter)
        (cons 'keymap keymap)
        (cons 'enable-hook enable-hook)
        (cons 'disable-hook disable-hook)
        (cons 'global? global?)))

(define (minor-mode-info-lighter info)
  (assq-ref info 'lighter))

(define (minor-mode-info-keymap info)
  (assq-ref info 'keymap))

(define (minor-mode-info-enable-hook info)
  (assq-ref info 'enable-hook))

(define (minor-mode-info-disable-hook info)
  (assq-ref info 'disable-hook))

(define (minor-mode-info-global? info)
  (assq-ref info 'global?))

(define (register-minor-mode mode-symbol info)
  "Register a minor mode with its metadata."
  (hash-set! minor-mode-table mode-symbol info))

(define (get-minor-mode-info mode-symbol)
  "Get metadata for a minor mode."
  (hash-ref minor-mode-table mode-symbol #f))

(define-syntax define-minor-mode
  (syntax-rules ()
    ((_ mode doc-string . options)
     (begin
       ;; Parse options
       (define mode-options (parse-minor-mode-options 'options))
       (define mode-lighter (assq-ref mode-options 'lighter))
       (define mode-keymap-name (assq-ref mode-options 'keymap))
       (define mode-enable-hook (assq-ref mode-options 'enable-hook))
       (define mode-disable-hook (assq-ref mode-options 'disable-hook))
       (define mode-global (assq-ref mode-options 'global))
       (define mode-body (assq-ref mode-options 'body))

       ;; Create the keymap variable if specified
       (when mode-keymap-name
         (define mode-keymap-name (make-sparse-keymap)))

       ;; Create the mode variable (global if :global #t, buffer-local otherwise)
       (if mode-global
           (define mode #f)
           (defvar-local mode #f doc-string))

       ;; Register the minor mode
       (register-minor-mode
        'mode
        (make-minor-mode-info
         mode-lighter
         (if mode-keymap-name mode-keymap-name #f)
         mode-enable-hook
         mode-disable-hook
         mode-global))

       ;; Define the toggle function
       (define (mode . args)
         doc-string
         (let ((arg (if (null? args) 'toggle (car args))))
           (minor-mode-toggle 'mode arg mode-body)))))))

(define (parse-minor-mode-options options)
  "Parse keyword options for define-minor-mode."
  (let loop ((opts options)
             (result '()))
    (if (null? opts)
        result
        (let ((key (car opts)))
          (cond
           ((eq? key ':lighter)
            (loop (cddr opts)
                  (acons 'lighter (cadr opts) result)))
           ((eq? key ':keymap)
            (loop (cddr opts)
                  (acons 'keymap (cadr opts) result)))
           ((eq? key ':enable-hook)
            (loop (cddr opts)
                  (acons 'enable-hook (cadr opts) result)))
           ((eq? key ':disable-hook)
            (loop (cddr opts)
                  (acons 'disable-hook (cadr opts) result)))
           ((eq? key ':global)
            (loop (cddr opts)
                  (acons 'global (cadr opts) result)))
           (else
            ;; Rest is body
            (acons 'body opts result)))))))

(define (minor-mode-toggle mode-symbol arg body)
  "Toggle or set a minor mode state."
  (let* ((info (get-minor-mode-info mode-symbol))
         (global? (and info (minor-mode-info-global? info)))
         (current-value (if global?
                           (let ((var (module-variable (current-module) mode-symbol)))
                             (if var (variable-ref var) #f))
                           (buffer-local-value mode-symbol (current-buffer))))
         (new-value (cond
                     ((eq? arg 'toggle) (not current-value))
                     ((number? arg) (> arg 0))
                     (else arg))))

    (when (not (eq? current-value new-value))
      (if new-value
          (minor-mode-enable mode-symbol info global? body)
          (minor-mode-disable mode-symbol info global?))

      ;; Return the new value
      new-value)))

(define (minor-mode-enable mode-symbol info global? body)
  "Enable a minor mode."
  ;; Set the variable
  (if global?
      (module-define! (current-module) mode-symbol #t)
      (buffer-set mode-symbol #t (current-buffer)))

  ;; Add to active minor modes list (C function) - only for buffer-local modes
  (unless global?
    (enable-minor-mode mode-symbol (current-buffer)))

  ;; Run body
  (when body
    (eval body (interaction-environment)))

  ;; Run enable hook
  (when (and info (minor-mode-info-enable-hook info))
    (let ((hook (minor-mode-info-enable-hook info)))
      (when (procedure? hook)
        (hook))))

  ;; Update mode line
  (force-mode-line-update))

(define (minor-mode-disable mode-symbol info global?)
  "Disable a minor mode."
  ;; Set the variable
  (if global?
      (module-define! (current-module) mode-symbol #f)
      (buffer-set mode-symbol #f (current-buffer)))

  ;; Remove from active minor modes list (C function) - only for buffer-local modes
  (unless global?
    (disable-minor-mode mode-symbol (current-buffer)))

  ;; Run disable hook
  (when (and info (minor-mode-info-disable-hook info))
    (let ((hook (minor-mode-info-disable-hook info)))
      (when (procedure? hook)
        (hook))))

  ;; Update mode line
  (force-mode-line-update))

;; Helper function for mode line
(define (force-mode-line-update)
  "Force the mode line to update."
  ;; TODO: Implement when mode line is available
  #t)

;; Query functions
(define (minor-mode-list)
  "Return list of all defined minor modes."
  (hash-map->list (lambda (k v) k) minor-mode-table))

(define (active-minor-modes . buf)
  "Return list of active minor modes in BUF (or current buffer)."
  (let ((buffer (if (null? buf) (current-buffer) (car buf))))
    (active-minor-modes-list buffer)))
