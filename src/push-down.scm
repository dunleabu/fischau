#!/usr/local/bin/guile \
-e main -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules
 ((fischau)))

(define (initial-state text)
  (list (string->list text) '()))


;; checks

(define (end-of-line state)
  (if (null? (car state)) state #f))

(define (open-bracket? state)
  (if (char=? #\( (caar state))
      state
      #f))

(define (not-close-bracket? state)
  (if (char=? #\) (caar state))
      #f
      state))

(define (empty-stack? state)
  (if (null? (cadr state))
      state
      #f))

;; update state

(define (next-char state)
  (list (cdar state) (cadr state)))

(define (push-stack state)
  (list (cdar state) (cons #t (cadr state))))

(define (pop-stack state)
  (list (cdar state) (cdadr state)))

(define (mark-failed state)
  (list (cdar state) (cons #f (cadr state))))

;; define push-down automaton

(define-graph g

  (node -> stop where end-of-line)
  (node -> node where open-bracket? then push-stack)
  (node -> node where not-close-bracket? then next-char)
  (node -> stop where empty-stack? then mark-failed)
  (node -> node then pop-stack)
  )

;; run it

(define (test-brackets! text)
  (display text)
  (newline)
  (let* ((s0 (initial-state text))
         (s1 (step-graph-recur g 'node s0)))
    (if (empty-stack? (cadr s1))
        (display " -> accepted")
        (display " -> failed"))
    (newline)))


(define (main args)
  (newline)
  (map test-brackets! (cdr args))
  (newline))

;(test "aa(b)dd")
