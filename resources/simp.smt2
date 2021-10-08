

(prog
    (set x 1)
    (set y x)
    (assert (>= y 1))
 )


(prog
    (when (< x 0)
        (set x (neg x))
    )
    (assert (>= x 0))
)



(check-sat)