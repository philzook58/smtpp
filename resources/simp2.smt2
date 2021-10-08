(define-method my-abs ((x Int))
    ;:pre true ; pre
    :post (>= x 0) ; post
    (when (< x 0)
        (set x (- x)))
)

(define-method my-abs-bad ((x Int))
   ; :pre true ; pre
    :post (>= x 1) ; post
    (when (< x 0)
        (set x (- x)))
)


(define-method my-sum ((x Int) (sum Int))
    :pre (> x 0) ; pre
    :post (>= sum 0) ; post
    (set sum 0)
    (while (> x 0)
           (>= sum 0) ;inv
           (set sum (+ sum x))
           (set x (- x 1))
    )
)
