
(add-tests-with-string-output "cons"
  [(fxadd1 0) => "1\n"]
  [(pair? (cons 1 2)) => "#t\n"]
  [(pair? 12) => "#f\n"]
  [(pair? #t) => "#f\n"]
  [(pair? #f) => "#f\n"]
  [(pair? ()) => "#f\n"]
  [(fixnum? (cons 12 43)) => "#f\n"]
  [(boolean? (cons 12 43)) => "#f\n"]
  [(null? (cons 12 43)) => "#f\n"]
  [(not (cons 12 43)) => "#f\n"]
  [(if (cons 12 43) 32 43) => "32\n"]
  [(car (cons 1 23)) => "1\n"]
  [(cdr (cons 43 123)) => "123\n"]
  [(car (car (cons (cons 12 3) (cons #t #f)))) => "12\n"]
  [(cdr (car (cons (cons 12 3) (cons #t #f)))) => "3\n"]
  [(car (cdr (cons (cons 12 3) (cons #t #f)))) => "#t\n"]
  [(cdr (cdr (cons (cons 12 3) (cons #t #f)))) => "#f\n"]
  [(let ([x (let ([y (fx+ 1 2)]) (fx* y y))])
     (cons x (fx+ x x)))
   => "(9 . 18)\n"]
  [(let ([t0 (cons 1 2)] [t1 (cons 3 4)])
     (let ([a0 (car t0)] [a1 (car t1)] [d0 (cdr t0)] [d1 (cdr t1)])
       (let ([t0 (cons a0 d1)] [t1 (cons a1 d0)])
         (cons t0 t1))))
   => "((1 . 4) 3 . 2)\n"]
  [(let ([t (cons 1 2)])
     (let ([t t])
       (let ([t t])
         (let ([t t])
           t))))
   => "(1 . 2)\n"]
  [(let ([t (let ([t (let ([t (let ([t (cons 1 2)]) t)]) t)]) t)]) t)
   => "(1 . 2)\n"]
  [(let ([x ()])
     (let ([x (cons x x)])
       (let ([x (cons x x)])
         (let ([x (cons x x)])
           (cons x x)))))
   => "((((()) ()) (()) ()) ((()) ()) (()) ())\n"]
  [(cons (let ([x #t]) (let ([y (cons x x)]) (cons x y)))
         (cons (let ([x #f]) (let ([y (cons x x)]) (cons y x)))
               ()))
   => "((#t #t . #t) ((#f . #f) . #f))\n"]
)
