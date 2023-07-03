(def fib-recursive
  (fn (x)
    (if (<= x 2)
      1
     (+ (fib-recursive (- x 1))
        (fib-recursive (- x 2))))))

(def fib-iter
  (fn (n) (fib-iter-helper 1 0 n)))

(def fib-iter-helper
  (fn (a b count)
      (if (= count 0)
          b
          (fib-iter-helper (+ a b) a (- count 1)))))

(def count 29)
(println (time (fib-recursive count)))
(println (time (fib-iter count)))
