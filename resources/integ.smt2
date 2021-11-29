(set-option :produce-proofs true)
(declare-sort Fun 0)
(declare-const COS Fun)
(declare-const SIN Fun)
(declare-const ID Fun)
(declare-fun CONST (Real) Fun)
(declare-fun COMP (Fun Fun) Fun)
(declare-fun DIFF (Fun) Fun)
(declare-fun PLUS (Fun Fun) Fun)
(declare-fun TIMES (Fun Fun) Fun)

(assert (distinct COS SIN ID))

(assert
    (= (DIFF ID) (CONST 0)) 
)

(assert
    (= (DIFF SIN) COS) 
)

(declare-fun apply (Fun Real) Real)
(declare-fun cos (Real) Real)
(declare-const Pi Real)
(assert (>= Pi 3.14))
(assert (<= Pi 3.15))
(assert (= (cos 0) 1))
(assert (= (cos Pi) -1))
(assert (= (cos (- Pi)) -1))
(assert (= (cos (* 2 Pi)) 1))
(assert (forall ((x Real)) (= (apply ID x) x)))
(assert (forall ((x Real)) (<= -1 (cos x))))
(assert (forall ((x Real)) (>= 1 (cos x))))

(assert
    (forall ((F Fun) (G Fun)) (= (DIFF (COMP F G)) (TIMES (COMP (DIFF F) G) (DIFF G))))
)

;(assert (not (exists ((F Fun))  (= F (DIFF (COMP ID ID)))   )))
(assert (not   (= (CONST 0) (DIFF (COMP ID ID)))   ))
(check-sat)
;(get-model)
(get-proof)

; so what is my question: Under what conditions. Is it possible to dump 