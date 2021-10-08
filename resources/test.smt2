(echo "verifying my-abs")
(push)
(declare-const x Int)
(assert
 (not
   (=> true
    (and (=> (< x 0) (let ((x (- x))) (>= x 0)))
     (=> (not (< x 0)) (>= x 0))))))

(check-sat)
(pop)
(get-model)