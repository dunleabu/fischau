#!/usr/local/bin/guile \
-e main -s
!#

(add-to-load-path (dirname (current-filename)))

(use-modules
 ((fischau)))

; lex state

(define (new-state chars history tokens)
  (list chars history tokens))

(define (initial-state text)
  (new-state (string->list text) '() '()))

(define get-chars car)
(define get-history cadr)
(define get-tokens caddr)


; functions to check current character against condition...

(define (wrap-check f)
  ;; wrap functions testing character value of (get-chars state)
  (lambda (state)
    (if (null? (get-chars state))
      #f
      (let ((char (car (get-chars state))))
        (if (f char) state #f)))))

;; simple character check functions

(define (char-alpha-or-underscore? char)
  (or (char=? #\_ char)
      (char-alphabetic? char)))

(define (char-alphanumeric-or-underscore? char)
  (or (char=? #\_ char)
      (char-alphabetic? char)
      (char-numeric? char)))

(define (char-dot? char) (char=? #\.))

(define (this-char target) (lambda (c) (char=? target c)))

(define (char-operator? char)
  ; return (truthy) symbol mappings for use below...
  (cond
    ((char=? #\+ char) #:plus)
    ((char=? #\- char) #:minus)
    ((char=? #\* char) #:mult)
    ((char=? #\/ char) #:div)
    ((char=? #\( char) #:lbrack)
    ((char=? #\) char) #:rbrack)
    (else #f)))

;; state check functions

(define is-number? (wrap-check char-numeric?))
(define is-var-head? (wrap-check char-alpha-or-underscore?))
(define is-var-tail? (wrap-check char-alphanumeric-or-underscore?))
(define is-dot? (wrap-check (this-char #\.)))
(define is-space? (wrap-check (this-char #\space)))
(define is-operator? (wrap-check char-operator?))
(define (reached-end? state)
  (if (null? (get-chars state)) state #f))

; state modification functions

(define (advance-drop-char state)
  (new-state
    (cdr (get-chars state))
    (get-history state)
    (get-tokens state)))

(define (advance state)
  (new-state
    (cdr (get-chars state))
    (cons (car (get-chars state)) (get-history state))
    (get-tokens state)))

(define (advance-push-operator state)
  (new-state
    (cdr (get-chars state))
    '()
    (cons (list #:op (char-operator? (car (get-chars state)))) (get-tokens state))))

(define (history->number history)
  (list #:number
        (string->number (list->string (reverse history)))))

(define (history->var history)
  (list #:var
        (list->string (reverse history))))

(define (push-number state)
  (new-state
    (get-chars state)
    '()
    (cons (history->number (get-history state)) (get-tokens state))))

(define (push-var state)
  (new-state
    (get-chars state)
    '()
    (cons (history->var (get-history state)) (get-tokens state))))



; lex graph

(define-graph g
  (start -> completed where reached-end?)
  (start -> integer where is-number? then advance)
  (start -> start where is-operator? then advance-push-operator)
  (start -> var where is-var-head? then advance)
  (start -> start where is-space? then advance-drop-char)

  (integer -> completed where reached-end? then push-number)
  (integer -> decimal where is-dot? then advance)
  (integer -> integer where is-number? then advance)
  (integer -> start where is-space? then push-number advance-drop-char)
  (integer -> start where is-operator? then push-number advance-push-operator)
  
  (decimal -> completed where reached-end? then push-number)
  (decimal -> decimal where is-number? then advance)
  (decimal -> start where is-space? then push-number advance-drop-char)
  (decimal -> start where is-operator? then push-number advance-push-operator)
  
  (var -> completed where reached-end? then push-var)
  (var -> var where is-var-tail? then advance)
  (var -> start where is-space? then push-var advance-drop-char)
  (var -> start where is-operator? then push-var advance-push-operator)
  )


(define (do-lex! text)
  (let* ((s (initial-state text))
         (res (step-graph-recur g 'start s)))
    (display text)
    (newline)
    (display "  => ")
    (display (reverse (get-tokens (cadr res))));(reverse (get-tokens res)))
    (newline)))


(define (main args) ;(do-lex! "aa + 56"))
  (newline)
  (map do-lex! (cdr args)))

