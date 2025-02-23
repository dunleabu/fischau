#!/usr/local/bin/guile \
-e main -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules
 ((fischau) #:prefix fsa:))

; acceptance function

(define (p< s n)
  ; if state < cumulative prob generate new random state else stop (#f)
  (if (< s n) (random 1.0) #f))

; apply functions (print to screen)

(define (zero! s)
  (display "0")
  s)

(define (one! s)
  (display "1")
  s)

(define (end! s)
  (newline)
  s)

; define graph given finish probability

(define (make-automaton p)
  (let* ((q2 (- 1.0 p))
        (q1 (* 0.5 q2)))
    (if (>= 0.0 p)
        (error "p must be > 0.0")
        (fsa:make-graph
         (start -> a where (p< 1.0) then one!)
         (a -> a where (p< q1) then one!)
         (a -> b where (p< q2) then zero!)
         (a -> finish where (p< 1.0) then end!)
         (b -> a where (p< 1.0) then one!)))))

; main routines

(define (run! finish-prob)
  (fsa:step-graph-recur (make-automaton (string->number finish-prob)) 'start 0.0)
  (newline))

(define (main args)
  (newline)
  (map run! (cdr args)))
