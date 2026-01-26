;;; timer.scm --- Timer functionality for Glemax

;;; Commentary:
;; This implements Emacs-style timers in pure Scheme.
;; Timers can run once or repeatedly at specified intervals.
;; The C code just needs to call `timer-event-handler' periodically.

;; Example usage:
;;
;; ;; Run once after 5 seconds
;; (run-at-time 5 #f (lambda () (message "Hello after 5 seconds")))
;;
;; ;; Run every 10 seconds
;; (run-at-time 10 10 (lambda () (message "Every 10 seconds")))
;;
;; ;; Run after 2 seconds, then every 5 seconds
;; (run-at-time 2 5 (lambda () (message "Started at 2, repeats every 5")))
;;
;; ;; Cancel a timer
;; (define my-timer (run-at-time 60 60 (lambda () (message "Tick"))))
;; (cancel-timer my-timer)

;;; Code:

;;; Timer structure
;; A timer is a list: (function interval next-run repeat?)
;; - function: the procedure to call
;; - interval: time in seconds between runs
;; - next-run: absolute time of next execution
;; - repeat?: whether to reschedule after running

(define timer-list '())
(set-var-doc! 'timer-list
  "List of all active timers.")

(define (make-timer function interval repeat?)
  "Create a new timer object.
FUNCTION is the procedure to call.
INTERVAL is the time in seconds.
REPEAT? determines if the timer should repeat."
  (list function
        interval
        (+ (current-time) interval)
        repeat?))

(define (timer-function timer)
  "Get the function from a timer."
  (car timer))

(define (timer-interval timer)
  "Get the interval from a timer."
  (cadr timer))

(define (timer-next-run timer)
  "Get the next run time from a timer."
  (caddr timer))

(define (timer-repeat? timer)
  "Check if timer should repeat."
  (cadddr timer))

(define (timer-set-next-run! timer time)
  "Set the next run time for a timer."
  (set-car! (cddr timer) time))

(define (run-at-time secs repeat function)
  "Run FUNCTION after SECS seconds.
If REPEAT is a number, repeat the timer every REPEAT seconds.
If REPEAT is #f, run only once.
Returns the timer object."
  (let* ((interval (if (number? repeat) repeat secs))
         (repeat? (and repeat #t))
         (timer (make-timer function interval repeat?)))
    (setq timer-list (cons timer timer-list))
    timer))

(define (run-with-timer secs repeat function)
  "Run FUNCTION after SECS seconds, repeating every REPEAT seconds if non-nil.
This is an alias for `run-at-time'."
  (run-at-time secs repeat function))

(define (run-with-idle-timer secs repeat function)
  "Run FUNCTION after SECS seconds of idle time.
If REPEAT is non-nil, repeat each time Glemax is idle for SECS seconds.
Note: Currently behaves the same as `run-at-time' until idle detection is implemented."
  ;; TODO: Implement proper idle detection
  (run-at-time secs repeat function))

(define (cancel-timer timer)
  "Cancel TIMER, preventing it from running."
  (setq timer-list (delq timer timer-list))
  #t)

(define (cancel-function-timers function)
  "Cancel all timers that would run FUNCTION."
  (setq timer-list
        (filter (lambda (timer)
                  (not (eq? (timer-function timer) function)))
                timer-list)))

(define (timer-event-handler)
  "Process pending timers. Called periodically by the C event loop.
This should be called at least once per second for reasonable timer accuracy."
  (let ((now (current-time)))
    (let loop ((timers timer-list)
               (remaining '()))
      (if (null? timers)
          (setq timer-list remaining)
          (let ((timer (car timers)))
            (if (>= now (timer-next-run timer))
                (begin
                  ;; Run the timer function
                  (catch #t
                    (lambda ()
                      ((timer-function timer)))
                    (lambda (key . args)
                      ;; Silently ignore errors in timer functions
                      ;; Could log to *Messages* if desired
                      #f))

                  ;; Reschedule if repeating
                  (if (timer-repeat? timer)
                      (begin
                        (timer-set-next-run! timer (+ now (timer-interval timer)))
                        (loop (cdr timers) (cons timer remaining)))
                      (loop (cdr timers) remaining)))
                ;; Timer not ready yet
                (loop (cdr timers) (cons timer remaining))))))))

(define (list-timers)
  "Return a list of all active timers."
  timer-list)

(define (timer-active? timer)
  "Return #t if TIMER is active (scheduled to run)."
  (memq timer timer-list))

;;; Utility functions

(define (run-with-timer-check interval function)
  "Run FUNCTION every INTERVAL seconds, but only if it returns #t.
Stops the timer if FUNCTION returns #f."
  (let ((timer #f))
    (setq timer
          (run-at-time interval interval
                       (lambda ()
                         (unless ((timer-function timer))
                           (cancel-timer timer)))))))

(define (run-at-time-with-delay secs delay repeat function)
  "Run FUNCTION after SECS+DELAY seconds, then repeat every REPEAT seconds.
Useful for scheduling a timer that starts later."
  (run-at-time (+ secs delay) repeat function))
