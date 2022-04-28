#lang play
(print-only-errors #t)
#|
<expr>   ::= <num>
           | <id>
           | <bool>
           | {cons <expr> <expr>}
           | {+ <expr> <expr>}
           | {< <expr> <expr>}
           | {= <expr> <expr>}
           | {fst <expr>}
           | {snd <expr>}
           | {if <expr> <expr> <expr>}
           | {with {{<id> <expr>}*} <expr>}
           | {<id> <expr>*}
|#
(deftype Expr
  (num n)
  (id x)
  (bool b)
  (pair f s)
  (add l r)
  (less l r)
  (eq l r)
  (fst e)
  (snd e)
  (if0 c t e)
  (with e b)
  (app f a))

#|
  <fundef> ::= {define {<id> <id>*} <expr>}
|#

(deftype FunDef
  (fundef name args expr))

;; <prog> ::= {<fundef>* <expr>}

#|
 parse :: s-expr -> Expr
 parsea una expresion concreta a una expresion abstracta
 genera las fundefs y el cuerpo del programa en sintaxis abstracta
|#
(define (parse c-expr)
  (match c-expr
    [(? number?) (num c-expr)]
    [(? symbol?) (id c-expr)]
    [(? boolean?) (bool c-expr)]
    [(list 'cons f s) (pair (parse f) (parse s))]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '< l r) (less (parse l) (parse r))]
    [(list '= l r) (eq (parse l) (parse r))]
    [(list 'fst x) (fst (parse x))]
    [(list 'snd x) (snd (parse x))]
    [(list 'if c t e) (if0 (parse c) (parse t) (parse e))]
    [(list 'with subs e) (with (map parse-subs subs) (parse e))]
    [(list 'define f b) (fundef (car f) (cdr f) (parse b))]
    [(list id a ...)  (app id (map parse a))]))

(define (parse-subs subs)
  (match subs
    [(list id val) (cons id (parse val))]))

; Tests parse
(test (parse '{with {{x 9} {y {cons 1 {cons 3 4}}}}
                    {sum x {fst y} {cadr y}}})
      (with (list (cons 'x (num 9)) (cons 'y (pair (num 1) (pair (num 3) (num 4)))))
            (app 'sum (list (id 'x) (fst (id 'y)) (app 'cadr (list (id 'y)))))))
(test (parse '{with {{x 5} {y 42} {z {cons 11 -3}}}
                     z})
      (with (list (cons 'x (num 5)) (cons 'y (num 42))
                        (cons 'z (pair (num 11) (num -3))))
            (id 'z)))
(test (parse '{add2 {triple 2}})
      (app 'add2 (list (app 'triple (list (num 2))))))


;; fundef parsing

(define (parse-fundef f)
  (match f
    [(list 'define (list name args ...) body) (fundef name args (parse body))]))

(test (parse-fundef '{define {f x y} {+ x y}})
      (fundef 'f (list 'x 'y) (add (id 'x) (id 'y))))

(test (parse-fundef '{define {g} {+ 1 2}})
      (fundef 'g '() (add (num 1) (num 2))))

(test (parse-fundef '{define {g x} {< x 2}})
      (fundef 'g (list 'x) (less (id 'x) (num 2))))

;; Environments

(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env mtEnv)
(define extend-env aEnv)

(define (lookup-env x env)
  (match env
    [(mtEnv) (error "free identifier:" x)]
    [(aEnv y v rest)
     (if (equal? y x)
         v
         (lookup-env x rest))]))

;; lookup-fundefs :: id List[fundef] -> fundef
(define (lookup-fundef f fundefs)
  (match fundefs
    ['() (error "undefined function" f)]
    [(cons val rest)
     (if (equal? (fundef-name f) val)
         f
         (lookup-fundef rest fundefs))]))
                    

#|
  interp :: expr env fundefs -> value (number)
  interpreta una expresion en un ambiente y con una lista de funciones definidas
|#
(define (interp expr env fundefs)
  (match expr
    [(num n) n]
    [(id x) (error "unbound identifier: " x)]
    [(bool b) b]
    [(pair f s) (pair (interp f env fundefs)
                      (interp s env fundefs))]
    [(add l r)
     (let ([x (interp l env fundefs)]
           [y (interp r env fundefs)])
       (if (number? x)
           (if (number? y)
               (+ x y)
               (error "expecter number, got " r))
           (error "expected number, got" l)))]
    [(less l r)
     (let ([x (interp l env fundefs)][y (interp r env fundefs)])
       (if (number? x)
           (if (number? y)
               (< x y)
               (error: "not a number: " r))
           (error "not a number: " l)))]
    [(eq l r )
     (let ([x (interp l env fundefs)][y (interp r env fundefs)])
       (if (number? x)
           (if (number? y)
               (equal? x y)
               (error: "not a number: " r))
           (error "not a number: " l)))]
    [(fst e) (if (pair? e) (interp (pair-f e) env fundefs) (error "Not a pair: " e))]
    [(snd e) (if (pair? e) (interp (pair-s e) env fundefs) (error "Not a pair: " e))]
    [(if0 c t e)
     (let ([cond (interp c env fundefs)])
       (if (boolean? cond)
           (if cond
               (interp t env fundefs)
               (interp e env fundefs))
           (error "if: condition is not bool" c)))]
    [(with e b)
     (interp b
             (foldl expand-env-pair env e)
             fundefs)]
    [(app f a)
     (def (fundef _ args b) (lookup-fundef f fundefs))
     (interp b
             
     ]))


;; expand-env :: pair -> env
(define (extend-env-pair p env)
  (extend-env (car p) (cdr p) env))

;;TODO : work 

     


;; Run program
(test (run '{{define {sum x y z} {+ x {+ y z}}}
             {define {cadr x} {fst {snd x}}}
             {with {{x 9} {y {cons 1 {cons 3 4}}}}
                   {sum x {fst y} {cadr y}} }})
      13)
(test (run '{{with {{x 5} {y 42} {z {cons 11 -3}}}
                   z}})
      ((pair 11 -3)))
(test (run '{{define {triple x} {+ x {+ x x}}}
             {define {add2 x} {+ 2 x}}
             {add2 {triple 2}}})
      8)
(test (run '{{with {{x 3} {y {+ 1 2}}}
                   {if {= x y} x y}}})
      3)

      
(test (run '{{with {{x 2} {y x} {z {+ 2 y}}}
                   {+ x {+ y z}}}})
      8)


