(DefMethod dummy ((x Int))
    ((post (>= x 0)))
    (Skip)
)

(DefMethod my-abs ((x Int))
    (
        (pre  true)
        (post (>= x 0))
    )
    ((If (< x 0)
        (Set x (- x))
        Skip
    ))
)