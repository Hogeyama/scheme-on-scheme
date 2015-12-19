
;{{{ stack
(define (stack-empty)
  (list))

(define (stack-push st val)
  (cons val st))

(define (stack-read st)
  (car st))

(define (stack-pop st)
  (cdr st))
;}}}
