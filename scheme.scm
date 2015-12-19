
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

;{{{binary-tree
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
;}}}

;{{{ 汎用
(define (fold f init lis)
  (if (null? lis)
      init
      (fold f (f init (car lis)) (cdr lis))))

(define (fold2 f init lis1 lis2)
  (if (or (null? lis1) (null? lis2))
      init
      (fold2 f (f init (car lis1) (car lis2))
               (cdr lis1)
               (cdr lis2))))
;}}}

;{{{util
(define (empty-frame) ;; frame
  (btree-empty)
  (list)
  )
;; `var'が既に`frame'内に定義されている場合は情報を上書きする。
(define (update frame var val) ;; frame -> var -> val -> frame
  (btree-insert var val frame)
  )
;; 見つからない場合は、#fを返す。
(define (lookup-frame var frame) ;; var -> frame -> Either bool (var, val)
  (btree-search var frame)
  )
;; env version
(define (lookup-env var env) ;; var -> env -> Either bool (var, val)
  (if (null? env)
      #f
      (let* ((top-frame (stack-read env))
             (found (lookup-frame var top-frame)))
        (if (not found)
            (lookup-env var (stack-pop env))
            found))))
(define (make-env) ;; env
  (cons (empty-frame) (stack-empty))
  )
;; 環境`env'を空のフレームで拡張した環境を返す。
(define (extend-env env) ;; env -> env
  (stack-push env (empty-frame))
  )
;; 環境`env'に変数`var'の値が`val'であるという情報を追加した環境を返す。
(define (define-var env var val) ;; env -> var -> val -> env
  (let* ((top-frame (stack-read env))
         (new-top-frame (update top-frame var val))
         (new-env (stack-push (stack-pop env) new-top-frame)))
    new-env
    ))
(define (define-var! env var val)
  (if (null? env)
    #f
    (set-car! env (update (car env) var val)))
  env)
;}}}

;{{{evaluaters :: exp -> env -> (env, val)
(define (app-eval exp env) ;{{{
  (let* ((l (repeat-base-eval exp env))
         (env (car l))
         (fun (cadr l))
         (args (cddr l)))
   (base-apply fun args env)))

(define (base-apply fun args env)
  (cond ((data-closure? fun)
          (if (= (length (closure-params fun)) (length args))
              (let* ((tmp-env (fold2 define-var! (extend-env (closure-env fun)) (closure-params fun) args))
                     (res     (base-eval (closure-body fun) tmp-env)))
                (cons env (cdr res)))
              (eval-error 'wrong-number-of-args fun env)))
        ((data-primitive? fun)
          (if (or (not (number? (primitive-arity fun)))
                  (= (primitive-arity fun) (length args)))
              ((primitive-fun fun) args env)
              (eval-error 'wrong-number-of-args fun env)))
        (else
          (eval-error 'non-function fun env))))

(define (repeat-base-eval explist env) ;; [exp] -> env -> (env, [val])
  (begin
    (define (f x exp) ;; (env, [exp]) -> exp -> (env, [exp])
      (let* ((env (car x))
             (res (base-eval exp env)))
        (cons (car res) (cons (cdr res) (cdr x)))))
    (let ((hoge (fold f (cons env '()) explist)))
      (cons (car hoge) (reverse (cdr hoge))))))

;}}}

(define (define-eval exp env);{{{
  (if (list? (cadr exp))
      (let* ((var    (car (cadr exp)))
             (params (cdr (cadr exp)))
             (body   (caddr exp))
             (val    (list '*lambda* env params body))
             (new-env (define-var! env var val)))
        (cons new-env #\#))
      (let* ((var (cadr exp))
             (val (cdr (base-eval (caddr exp) env)))
             (new-env (define-var! env var val)))
        (cons new-env #\#))))
;}}}

(define (let-eval exp env);{{{
  (begin
    (define (let->app exp)
      (let* ((decl (cadr exp))
             (params (map car decl))
             (args   (map cadr decl))
             (body (caddr exp)))
        (cons (list 'lambda params body) args)))
    (base-eval (let->app exp) env)))
;}}}

(define (lambda-eval exp env);{{{
  (let* ((params (cadr exp))
         (body   (caddr exp)))
    (cons env (list '*lambda* env params body))))
;}}}

(define (var-eval exp env);{{{
  (let ((found (lookup-env exp env)))
    (if (not found)
        (cons env '*unspecified*)
        (cons env (cdr found))
        )))
;}}}

(define (begin-eval exp env);{{{
  (let* ((res (repeat-base-eval (cdr exp) env))
         (env (car res))
         (vl (reverse (cdr res))))
    (cons env (if (null? vl) #t (car vl)))))
;}}}

(define (if-eval exp env);{{{
  (let* ((res (base-eval (cadr exp) env)))
    (if (cdr res)
        (base-eval (caddr  exp) env)
        (base-eval (cadddr exp) env)
      )))
;}}}

(define (cond-eval exp env);{{{
  (begin
    (define (cond-eval-sub exp env)
      (let* ((head (car exp))
             (res  (base-eval (car head) env)))
        (if (or (cdr res) (equal? (cdr res) 'else))
            (begin
              ;(display (cadr head))
              ;(newline)
              ;(display (cdr  res))
              (base-eval (cadr head) (car res))
            )
            (cond-eval-sub (cdr exp) env))))
    (cond-eval-sub (cdr exp) env)))
;}}}

(define (quote-eval exp env);{{{
  (cons env (cadr exp)))
;}}}

(define (list-eval exp env);{{{
  (let ((vals (cdr (repeat-base-eval (cdr exp) env))))
    (cons env vals)))
;}}}

(define (let*-eval exp env);{{{
  (begin
    (define (let*-eval-sub li body)
      (if (null? li)
          body
          (let* ((head (car li))
                 (tail (cdr li)))
            (list 'let (list head)
                  (let*-eval-sub tail body)))))
    (let ((exp- (let*-eval-sub (cadr exp) (caddr exp))))
      (let-eval exp- env))))
;}}}

(define (eval-error type exp env);{{{
  (display "ERROR: ")
  (write type)
  (display ": ")
  (print-data exp)
  (newline)
  (cons env '*error*))
;}}}

;}}}

;{{{base-eval  :: exp -> env -> (env, val)
(define (base-eval exp env)
  (cond
    ((equal? exp 'env)          (display env)              (cons env ""))
    ((equal? exp 'frame)        (display (stack-read env)) (cons env ""))
    ((eof-object? exp)          (cons env '*exit*))
    ((constant? exp)            (cons env exp))
    ((symbol? exp)              (var-eval exp env))
    ((not (pair? exp))          (eval-error 'non-evaluatable exp env))
    ((equal? (car exp) 'exit)   (cons env '*exit*))
    ((equal? (car exp) 'define) (define-eval exp env))
    ((equal? (car exp) 'let)    (let-eval    exp env))
    ((equal? (car exp) 'let*)   (let*-eval   exp env))
    ((equal? (car exp) 'letrec) (letrec-eval exp env))
    ((equal? (car exp) 'lambda) (lambda-eval exp env))
    ((equal? (car exp) 'if)     (if-eval     exp env))
    ((equal? (car exp) 'cond)   (cond-eval   exp env))
    ((equal? (car exp) 'begin)  (begin-eval  exp env))
    ((equal? (car exp) 'quote)  (quote-eval  exp env))
    ((equal? (car exp) 'list)   (list-eval   exp env))
    (else (app-eval exp env))))
;}}}

;{{{ print-data
(define (print-data data)
  (cond
    ((data-closure? data)         (display "#<closure>"))
    ((data-primitive? data)       (display "#<primitive>"))
    ((equal? data '*unspecified*) (display "#<unspecified>"))
    ((equal? data '*error*)       (display "#<error>"))
    ((equal? data '*exit*))
    (else (write data))))
;}}}

;{{{primitive他
(define (constant? exp)
  (or (boolean? exp) (number? exp) (string? exp)))

(define (make-primitive arity fun)
  (list '*primitive* arity fun))
(define (data-primitive? data)
  (and (pair? data) (equal? (car data) '*primitive*)))
(define primitive-arity cadr) ;;引数
(define primitive-fun caddr)  ;;本体

(define (data-closure? data)
  (and (pair? data) (equal? (car data) '*lambda*)))
(define closure-env cadr)
(define closure-params caddr)
(define closure-body cadddr)

(define (make-top-env)
  (let* ((env (make-env))
         (env (define-var! env '=
                (make-primitive 2
                  (lambda (args env) (cons env (= (car args) (cadr args)))))))
         (env (define-var! env '<
                (make-primitive 2
                  (lambda (args env) (cons env (< (car args) (cadr args)))))))
         (env (define-var! env '>
                (make-primitive 2
                  (lambda (args env) (cons env (> (car args) (cadr args)))))))
         (env (define-var! env '+
                (make-primitive 2
                  (lambda (args env) (cons env (+ (car args) (cadr args)))))))
         (env (define-var! env '-
                (make-primitive 2
                  (lambda (args env) (cons env (- (car args) (cadr args)))))))
         (env (define-var! env '*
                (make-primitive 2
                  (lambda (args env) (cons env (* (car args) (cadr args)))))))
         (env (define-var! env 'equal?
                (make-primitive 2
                  (lambda (args env) (cons env (equal? (car args) (cadr args)))))))
         (env (define-var! env 'and
                (make-primitive 2
                  (lambda (args env) (cons env (and (car args) (cadr args)))))))
         (env (define-var! env 'or
                (make-primitive 2
                  (lambda (args env) (cons env (or (car args) (cadr args)))))))
         (env (define-var! env 'not
                (make-primitive 1
                  (lambda (args env) (cons env (not (car args)))))))
         (env (define-var! env 'cons
                (make-primitive 2
                  (lambda (args env) (cons env (cons (car args) (cadr args)))))))
         (env (define-var! env 'car
                (make-primitive 1
                  (lambda (args env) (cons env (car (car args)))))))
         (env (define-var! env 'cdr
                (make-primitive 1
                  (lambda (args env) (cons env (cdr (car args)))))))
         (env (define-var! env 'cadr
                (make-primitive 1
                  (lambda (args env) (cons env (cadr (car args)))))))
         (env (define-var! env 'caddr
                (make-primitive 1
                  (lambda (args env) (cons env (caddr (car args)))))))
         (env (define-var! env 'cadddr
                (make-primitive 1
                  (lambda (args env) (cons env (cadddr (car args)))))))
         (env (define-var! env 'null?
                (make-primitive 1
                  (lambda (args env) (cons env (null? (car args)))))))
         (env (define-var! env 'pair?
                (make-primitive 1
                  (lambda (args env) (cons env (pair? (car args)))))))
         (env (define-var! env 'list?
                (make-primitive 1
                  (lambda (args env) (cons env (list? (car args)))))))
         (env (define-var! env 'symbol?
                (make-primitive 1
                  (lambda (args env) (cons env (symbol? (car args)))))))
         (env (define-var! env 'number?
                (make-primitive 1
                  (lambda (args env) (cons env (number? (car args)))))))
         (env (define-var! env 'boolean?
                (make-primitive 1
                  (lambda (args env) (cons env (boolean? (car args)))))))
         (env (define-var! env 'string?
                (make-primitive 1
                  (lambda (args env) (cons env (string? (car args)))))))
         (env (define-var! env 'string<?
                (make-primitive 1
                  (lambda (args env) (cons env (string<? (car args)))))))
         (env (define-var! env 'eof-object?
                (make-primitive 1
                  (lambda (args env) (cons env (eof-object? (car args)))))))
         (env (define-var! env 'display
                (make-primitive 1
                  (lambda (args env) (cons env (display (car args)))))))
         (env (define-var! env 'write
                (make-primitive 1
                  (lambda (args env) (cons env (write (car args)))))))
         (env (define-var! env 'read
                (make-primitive 1
                  (lambda (args env) (cons env (read (car args)))))))
         (env (define-var! env 'length
                (make-primitive 1
                  (lambda (args env) (cons env (length (car args)))))))
         (env (define-var! env 'map
                (make-primitive 2
                  (lambda (args env) (cons env (map (car args) (cadr args)))))))
         (env (define-var env 'load
                (make-primitive 1
                  (lambda (args env)
                    (with-input-from-file (car args)
                      (lambda ()
                        (define (re-loop env)
                          (let* ((res (base-eval (read) env))
                                 (env (car res))
                                 (val (cdr res)))
                            (if (equal? val '*exit*)
                              (cons env '*unspecified*)
                              (re-loop env))))
                        (re-loop env)))))))
         (env (define-var! env 'set-car!
                (make-primitive 2
                  (lambda (args env) (cons env (set-car! (car args) (cadr args)))))))
         (env (define-var env 'test
                (make-primitive 0
                  (lambda (args env)
                    (with-input-from-file "test.scm"
                      (lambda ()
                        (define (re-loop env)
                          (let* ((res (base-eval (read) env))
                                 (env (car res))
                                 (val (cdr res)))
                            (if (equal? val '*exit*)
                              (cons env '*unspecified*)
                              (re-loop env))))
                        (re-loop env)))))))
         )
    env
  ))
;}}}

;{{{main
(define (scheme)
  (let ((top-env (make-top-env)))
    (begin
      (define (rep-loop env)
        (begin
          (display "> ")
          (let* ((res (base-eval (read) env))
                 (env (car res))
                 (val (cdr res)))
            (begin
              (print-data val)
              (newline)
              (if (equal? val '*exit*)
                  #t
                  (rep-loop env))))))
      (rep-loop top-env))))
;}}}


