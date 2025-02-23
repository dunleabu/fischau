(define-module (fischau)
  #:export (make-graph define-graph ste-graph step-graph-n step-graph-recur step-graph-verbose))

(use-modules (rnrs hashtables ))

; code for graph defining macros

(define-syntax insert-arg
  (syntax-rules ()
    ((_ a (f . xs))
     (f a . xs))
    ((_ a f)
     (f a))
    ((_ a f . fs)
     (insert-arg (insert-arg a f) . fs))

    ))

(define-syntax chain-lambda
  (syntax-rules ()
    ((_) identity)
    ((_ . xs)
     (lambda (a) (insert-arg a . xs)))))

(define-syntax edge
  (syntax-rules (-> where then)
    ((_ x -> y)
     (list 'x 'y identity identity))
    ((_ x -> y then . z)
     (list 'x 'y identity (chain-lambda . z)))
    ((_ x -> y where condition)
     (list 'x 'y (chain-lambda condition) identity))
    ((_ x -> y where condition then . z)
     (list 'x 'y (chain-lambda condition) (chain-lambda . z)))
    ))

(define (get-node table key)
  (let ((val (hashtable-ref table key #f)))
    (if val
        val
        (let ((inner-table (make-eq-hashtable)))
          (hashtable-set! table key inner-table)
          inner-table))))

(define (add-edge table data)
  (let ((key-from (car data))
        (key-to (cadr data))
        (accept-func (caddr data))
        (apply-funcs (cadddr data)))
    (hashtable-update! table key-from (lambda (val) (cons (list key-to accept-func apply-funcs) val)) '())))

(define (create-graph-from-edges edges)
  (let ((output (make-eq-hashtable)))
    (map (lambda (x) (add-edge output x)) (reverse edges))
    output))

(define-syntax edges-from-list
  (syntax-rules ()
    ((_ e)
     (list (edge . e)))
    ((_ e1 e2 ...)
     (cons (edge . e1) (edges-from-list e2 ...)))
    ))

(define-syntax make-graph
  (syntax-rules ()
    ((_ . xs)
     (create-graph-from-edges (edges-from-list . xs)))))

(define-syntax define-graph
  (syntax-rules()
    ((_ name . xs)
     (define name (make-graph . xs)))))

; code for graph stepping functions

;; get things from first edge in list
(define get-symbol caar)
(define get-accept-function cadar)
(define get-apply-function caddar)

(define get-remaining-edges cdr)

(define (run-edges edges state)
  (if (null? edges)
      (list #f state)
      (let ((accept-state ((get-accept-function edges) state)))
        (if accept-state
            (list (get-symbol edges) ((get-apply-function edges) accept-state))
            (run-edges (get-remaining-edges edges) state)))))

(define (step-graph graph node state)
  (let ((edges (hashtable-ref graph node #f)))
    (if edges
        (run-edges edges state)
        (list #f state))))

(define (step-graph-n graph node state n)
  (if (> 1 n)
      (list node state)
      (let* ((res (step-graph graph node state))
             (new-node (car res))
             (new-state (cadr res)))
        (if new-node
            (step-graph-n graph new-node new-state (- n 1))
            (list new-node new-state)))))

(define (step-graph-recur graph node state)
  (let* ((res (step-graph graph node state))
         (new-node (car res))
         (new-state (cadr res)))
    (if new-node
        (step-graph-recur graph new-node new-state)
        (list new-node new-state))))

(define (step-graph-verbose graph node state)
  (let* ((res (step-graph graph node state))
         (new-node (car res))
         (new-state (cadr res)))
    (display node)
    (display " => ")
    (display new-node)
    (newline)
    (display "    ")
    (display state)
    (display " => ")
    (display new-state)
    (newline)
    (if new-node
        (step-graph-verbose graph new-node new-state)
        (list new-node new-state))))

;;; test code


(define (log x)
  (display x)
  (newline)
  x)

(define (is-one s) (if (eq? s 1) s #f))

(define-graph g
 (a -> b)
 (b -> c where is-one then log (+ 1) log)
 (c -> d)
 )



