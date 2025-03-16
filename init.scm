(use-modules (glemax))

;; Create and switch buffers
;; (buffer-new "*buffer-name*" "" "default-font")
;; (buffer-switch "*buffer-name*")

;; Split windows
;; (window-split-horizontal)
;; (window-focus-next 1)


(define-syntax-rule (interactive)
  (let ((proc-name (procedure-name (current-procedure))))
    (when proc-name; Only if we can get the name
      (register-command 
       (symbol->string proc-name)
       "Interactive function"
       (string-append "(" (symbol->string proc-name) ")")))))

(define (scheme-command)
  ;; (interactive)
  (display "Hello from Scheme command!"))

(register-command "scheme-command" 
                  "A custom command defined in Scheme"
                  "(scheme-command)")





;; Show messages
(message "Loaded init.scm")
