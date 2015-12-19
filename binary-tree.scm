;;;実装間違ってるっぽい

(define (btree-empty)
  '())

(define (btree-null? t)
  (null? t))

(define (btree-search key t)
  (if (btree-null? t)
      #f
      (let ((parent (car t))
            (childL (car (cdr t)))
            (childR (cdr (cdr t))))
        (cond
          ((equal? key (car parent))
                parent)
          ((symbol<? key (car parent))
                (btree-search key childL))
          (else
                (btree-search key childR))))))

(define (btree-insert key val t)
  (if (btree-null? t)
      (mktree (cons key val) (btree-empty) (btree-empty))
      (let ((parent (car t))
            (left-child  (car (cdr t)))
            (right-child (cdr (cdr t))))
        (cond
          ((equal? key (car parent))
            (mktree (cons key val) left-child right-child))
          ((symbol<? key (car parent))
            (let ((new-left-child (btree-insert key val left-child)))
               (mktree parent new-left-child right-child)))
          (else
            (let ((new-right-child (btree-insert key val right-child)))
               (mktree parent left-child new-right-child)))))))

(define (btree-delete key t)
  (if (btree-null? t)
      t
      (let ((parent (car t))
            (left-child  (car (cdr t)))
            (right-child (cdr (cdr t))))
        (cond
          ((equal? key (car parent))
              (cond
                ((btree-null? left-child ) right-child)
                ((btree-null? right-child) left-child )
                (else (let ((hoge (delmax left-child)))
                      (mktree (car hoge) (cdr hoge) right-child)))))
          ((symbol<? key (car parent))
            (let ((new-left-child (btree-delete key left-child)))
               (mktree parent new-left-child right-child)))
          (else
            (let ((new-right-child (btree-delete key right-child)))
               (mktree parent left-child new-right-child)))))))

;;木の最大要素と，それを削除したのこりのペアを返す．
(define (delmax t)
  (let ((parent (car t))
        (left-child  (car (cdr t)))
        (right-child (cdr (cdr t))))
    (if (btree-null? right-child)
        (cons parent left-child)
        (let ((hoge (delmax right-child)))
          (cons (car hoge) (mktree parent left-child (cdr hoge))))
        )))

(define (mktree hoge left-child right-child)
  (cons hoge (cons left-child right-child)))

(define (symbol<? s1 s2)
  (let ((str1 (symbol->string s1))
        (str2 (symbol->string s2)))
    (string<? str1 str2)
    ))


;
;(let ((t (btree-empty)));{{{
;  (define (input->string x)
;    (cond
;      ((symbol? x) (symbol->string x))
;      ((number? x) (number->string x))
;      ((string? x) x)
;      (else #f)
;      ))
;  (define (main-loop t)
;    (let ((cmd (read)))
;      (cond
;        ((equal? cmd 'insert)
;          (let* ((key (input->string (read)))
;                 (val (input->string (read))))
;            (main-loop (btree-insert key val t))))
;        ((equal? cmd 'delete)
;          (let* ((key (input->string (read))))
;            (main-loop (btree-delete key t))))
;        ((equal? cmd 'search)
;          (let* ((key (input->string (read)))
;                 (entry (btree-search key t)))
;            (if (not entry)
;                (display "(not found)\n")
;                (begin
;                  (display (cdr entry))
;                  (newline)))
;            (main-loop t)))
;        ((or (equal? cmd 'quit) (eof-object? cmd))
;           #t)
;        (else
;          (display "(unknown command)\n")
;          (main-loop t)))))
;  (main-loop t));}}}
;
