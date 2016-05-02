(ns epicea.breakable-test
  (:require [clojure.test :refer :all]
            [epicea.breakable :refer :all]))

(defn call-counter []
  (let [n (atom -1)]
    (fn [x]
      (let [v (swap! n inc)]
        (assert (= v x))
        x))))

;; [[2 3 4] [5 6 7]]*[3 4 5] = [38 74]

(defn matmul [mat0 v0]
  (loop [mat mat0
         result []]
    (if (empty? mat)
      result
      (recur (rest mat)
             (conj result
                   (loop [v v0
                          r (first mat)
                          s 0]
                     (if (empty? r)
                       s
                       (recur (rest v) (rest r) (+ s (* (first v) (first r)))))))))))

(defn matmul3 [mat0 v0]
  (breakable 
    (loop [mat (expect sequential? mat0 :a)
           result []]
      (if (empty? (expect sequential? mat :b))
        result
        (recur (rest mat)
               (conj result
                     (loop [v v0
                            r (first mat)
                            s 0]
                       (if (empty? r)
                         s
                         (recur (rest v) 
                                (rest r) 
                                (+ s 
                                   (* (expect number? (first v) :not-a-number)
                                      (first r))))))))))))

(deftest malen
  (testing "stuff"
    (is (= 10 (eval (build-fun-call-sub '(+ 2 3) [] (fn [x] `(* 2 ~x))))))
    (is (= 18 (eval (build-fun-call-sub '(+ (+ 2 4) 3) [] (fn [x] `(* 2 ~x))))))
    (is (= 72 (eval (build-fun-call-sub `(+ 2 ~(fn [cb] (cb 34))) [] (fn [x] `(* 2 ~x))))))
    (is (= 26 (eval ((to-fun (build-fun-call '(+ 1 2 3 (* 4 5)) [nil])) identity))))
    (is (= 4 (eval (make-otherwise-expr ['inc] 3))))
    (is (= 119 (eval ((build-expect '(expect number? 119 :a) [:b]) identity))))
    (is (= :b (eval ((build-expect '(expect number? "kattskit" :a) [:b]) identity))))
    (is (= :a (eval ((build-expect '(expect number? "kattskit" :a) [])
                     identity))))
    (is (= :k (eval ((build-expect '(expect number? "kattskit") [:k]) identity))))
    (is (= 3 (eval ((build '(expect number? 3) [nil]) identity))))
    (is (= 7 (eval ((build '(+ (expect number? 3 :a) (expect number? 4 :b)) [nil]) identity))))
    (is (= 14 (eval ((build '(+ (expect number? 
                                        (+ 3 7) :a) (expect number? 4 :b)) 
                            [nil]) identity))))
    (is (= :a (eval ((build-if '(if (expect number? 4) :a :b) [nil]) identity))))
    (is (= :b (eval ((build-if '(if (expect nil? nil) :a :b) [nil]) identity))))
    (is (= :a (eval ((build '(if (= 9 (+ (expect number? 3) 
                                         (expect number? 6))) :a :b) [nil]) identity))))
    (is (= :c (eval ((build '(if (= 9 (+ (expect number? "c") 
                                         (expect number? 6))) :a :b) [:c]) identity))))
    (is (= :kattskit (eval ((build '(if (number? :a) 
                                      (expect number? :b) 
                                      (expect number? :c)) [:kattskit]) 
                            identity))))
    (is (= 9 (eval ((build '(if (number? :a) 
                              (assert false) 
                              (expect number? 9)) [:kattskit]) identity))))
    (is (= 13 (eval ((build '(+ 4 (if (expect number? 3) 9 12)) [:kattskit]) identity))))
    (is (= 6 (eval ((build-do '(do 1 (expect number? 2) 3 4 5 6) [nil]) identity))))
    (is (= :kattskit (eval ((build-do '(do 1 (expect number? :a) 3 4 5 6) [:kattskit]) identity))))
    (is (= 42 (eval ((build '(* (do 1 9 (expect number? 12) 7) 
                                (do 1 (expect number? 9) 3 4 5 6)) [:kattskit]) identity))))
    (is (= [[1 2] [3 4]] (make-pairs [1 2 3 4])))
    (is (= 4 (eval (make-let 'a 4 'a))))
    (is (= 9 (eval (make-let 'a 4 '(let [b 5] (+ a b))))))
    (is (= 9 (eval ((to-fun (build-let `(let [] 9) [nil])) identity))))
    (is (= 7 (debug-build (let [a 3 b 4] (+ a b)))))
    (is (= 7 (debug-build (let [a 3 b (expect number? 4)] (+ a b)))))
    (is (= :otherwise-branch (debug-build (let [a 3 b (expect number? :a)] (+ a b)))))
    (is (= 196 (debug-build (* (let [a (expect number? (+ 3 4))] (* 2 a))
                               (let [b (expect number? (+ 2 3))] (+ 9 b))))))
    (is (= 9 (call-cb (fn [x] (* 3 x)) 3)))
    (let [c (call-counter)]
      (is (= (debug-build (* (+ (expect number? (c 0))
                                (expect number? (c 1)))
                             (+ (expect number? (c 2))
                                (expect number? (c 3)))))
             5)))
    (let [c (call-counter)]
      (is (= 196 (debug-build (* (let [a (do (c 0) (expect number? (+ 3 4)))] 
                                   (do (c 1) (* 2 a)))
                                 (let [b (do (c 2) (expect number? (+ 2 3)))] 
                                   (do (c 3) (+ 9 b))))))))

    (let [c (call-counter)]
      (is (= [0 1 2 3] (debug-build [(c 0) (c 1) (expect number? (c 2)) (c 3)]))))

    (let [c (call-counter)] ;; No point in checking the order here.
      (is (= #{0 1 2 3} (debug-build #{0 1 (expect number? 2) 3}))))
    (is (= :kattskit (eval ((build '{:a 2 :b (expect number? :c)} [:kattskit]) identity))))
    (is (= {:a 2 :b 3} (eval ((build '{:a 2 :b (expect number? 3)} [:kattskit]) identity))))

    ;; (is (= 30 (eval ((build '(let [a 3]
    ;;                            (+ a (loop [b 4
    ;;                                        a 1]
    ;;                                   (if (= 0 b)
    ;;                                     (+ 3 a)
    ;;                                     (recur (- b 1) (* a b)))))) nil) identity))))

    (is (= :b (breakable (+ 3 4 5 (expect number? :a)) :otherwise :b)))
    (is (= :a (breakable (+ 3 4 5 (expect number? :a)))))
    (is (= 20 (breakable (+ 3 4 5 (expect number? 8)))))
    (is (= 20 (breakable (+ 3 4 5 8))))

    (let [c (call-counter)]
      (is (= 140 (breakable (let [a (c 0)
                                   b (c 1)]
                               (if (= (+ a b (c 2)) 4)
                                 (c 119)
                                 (* (+ (c 3) (let [k (c 4)
                                                   z (do (+ 2 (c 5)))]
                                               (+ k z (c 6))))
                                    (c 7))))))))

    (let [c (call-counter)]
      (is (= (breakable (+ (c 0) (let [a (c 1) b (c 2)] (+ a b (c 3))) (c 4)))
             10)))

    (let [c (call-counter)]
      (is (= (breakable (+ (c (expect number? 0)) (let [a (c 1) b (c 2)] (+ a b (c 3))) (c 4)))
             10)))

    (let [c (call-counter)]
      (is (= (breakable (+ (c 0) (let [a (c (expect number? 1)) b (c 2)] (+ a b (c 3))) (c 4)))
             10)))

    (let [c (call-counter)]
      (is (= (breakable (+ (c 0) (let [a (c 1) b (c (expect number? 2))] (+ a b (c 3))) (c 4)))
             10)))

    (let [c (call-counter)]
      (is (= (breakable (+ (c 0) (let [a (c 1) b (c 2)] (+ a (expect number? b) (c 3))) (c 4)))
             10)))

    (let [c (call-counter)]
      (is (= (breakable (+ (c 0) (let [a (c 1) b (c 2)] (+ a b (c (expect number? 3)))) (c 4)))
             10)))

    (let [c (call-counter)]
      (is (= (breakable (+ (c 0) (let [a (c 1) b (c 2)] (+ a b (c 3))) (c (expect number? 4))))
             10)))

    (let [c (call-counter)]
      (is (= (breakable (+ (c 0) (let [a (c 1) b (c 2)] 
                                     (+ (expect number? (+ a b)) (c 3))) (c (expect number? 4))))
             10)))

    (let [c (call-counter)]
      (is (= (breakable (+ (c 0) (let [a (c 1) b (c 2)] (+ a b (c 3))) (c 4)))
             10)))

    (let [c (call-counter)]
      (is (= (breakable (+ (expect number? (c 0)) (let [a (c 1) b (c 2)] (+ a b (c 3))) (c 4)))
             10)))

    (let [c (call-counter)]
      (is (= (breakable (+ (c 0) (let [a (expect number? (c 1)) b (c 2)] (+ a b (c 3))) (c 4)))
             10)))

    (let [c (call-counter)]
      (is (= (breakable (+ (c 0) (let [a (c 1) b (expect number? (c 2))] (+ a b (c 3))) (c 4)))
             10)))

    (let [c (call-counter)]
      (is (= (breakable (+ (c 0) (let [a (c 1) b (c 2)] (+ a b (expect number? (c 3)))) (c 4)))
             10)))

    (let [c (call-counter)]
      (is (= (breakable (+ (c 0) (let [a (c 1) b (c 2)] (+ a b (c 3))) (expect number? (c 4))))
             10)))

    (let [c (call-counter)]
      (is (= (breakable (+ (c 0) (let [a (c 1) b (c 2)] 
                                     (+ (expect number? (+ a b)) (c 3))) (expect number? (c 4))))
             10)))


    (is (not (fn? (build '(let [a 3 b 4] (do (+ a b) 9)) [nil]))))
    (is (fn? (build '(let [a 3 b 4] (do (+ a b) (expect number? 9))) [nil])))
    (is (not (fn? (build '(do (do (do (let [a 3 b 4] 
                                        (do (+ a (+  9 9 b)) 
                                            (let [k 9] 9))  9)))) [nil]))))

    (is (contains-recursive? [[[[:mjao]]]] #{:mjao}))
    (is (not (contains-recursive? [[[[:mja]]]] #{:mjao})))
    (is (contains-recursive? [[[[{:a [:b 13 4 5 #{:a :b :mjao}]}]]]] #{:mjao}))

    (is (try
          (build '(loop [a 1 b 2] (malen.core/expect number? 9)) [nil])
          false
          (catch Throwable e
            true)))

    (let [c (call-counter)]
      (is (= 4 (breakable
                 (c 0)
                 (c 1)
                 (do
                   (expect number? (c 2))
                   (c 3))
                 (c 4)))))

    (let [c (call-counter)]
      (is (= 4 (breakable
                 (c 0)
                 (c 1)
                 (if (= 2 (c 2))
                   (do (c 3)
                       4)
                   (do (c 3)
                       5))))))

    (is (try 
          (breakable
            (throw (do (expect number? :a)
                       (RuntimeException. "Kattskit"))))
          true

          (catch RuntimeException e
            false)))

    (is (try 
          (breakable
            (throw (do (expect number? 992)
                       (RuntimeException. "Kattskit"))))
          false

          (catch RuntimeException e
            true)))

    (is (= [[1 2 3] [:a :b]]
           (read-while number? [1 2 3 :a :b])))

    (is (= 9 (breakable (try (assert false) (catch Throwable e 9)))))
    (let [x (atom [])]
      (is (= 13 (breakable (try 
                              (assert false) 
                              (catch Throwable e 
                                (swap! x (fn [y] (conj y 9)))
                                13)
                              (finally
                                (swap! x (fn [y] (conj y 11))))))))
      (is (= (deref x) [9 11])))

    (let [x (atom [])]
      (is (= 13 (try 
                  (assert false) 
                  (catch Throwable e 
                    (swap! x (fn [y] (conj y 9)))
                    13)
                  (finally
                    (swap! x (fn [y] (conj y 11))))))))

    (is (= 9 (breakable (try (assert false) (catch Throwable e 9)))))
    (let [x (atom [])]
      (is (= 13 (breakable (try 
                              (assert false) 
                              (expect number? :a)
                              (catch Throwable e 
                                (swap! x (fn [y] (conj y 9)))
                                (expect number? 13))
                              (finally
                                (expect number? :b)
                                (swap! x (fn [y] (conj y 11))))))))
      (is (= (deref x) [9])))

    (is (= 16 (breakable (let [a 9 b 7] (cancel (+ a b)) 119))))

    (is (= (loop [n 9
                  result 1]
             (breakable
               (recur (- n 1)
                      (* result (expect (fn [x] (< 0 x)) n result)))))
           362880))


;; [[2 3 4] [5 6 7]]*[3 4 5] = [38 74]
    ;; (is (= [38 74] (matmul [[2 3 4] [5 6 7]] [3 4 5])))
    ;; (is (= [38 74] (matmul3 [[2 3 4] [5 6 7]] [3 4 5])))

))



;; (let* [G__6832 (+ 3 4)] 
;;       (if (number? G__6832) 
;;         (clojure.core/let [a G__6832 
;;                            G__6833 (+ 2 3)] 
;;           (if (number? G__6833) 
;;             (clojure.core/let [b G__6833 
;;                                G__6834 (* (do (* 2 a)) (do (+ 9 b)))] 
;;               G__6834) :otherwise-branch)) :otherwise-branch))

;; ;; SETS ARE UNORDERED
;; malen.core-test> (defmacro disp-macro [x] (println "x = " x) x)
;; #'malen.core-test/disp-macro
;; malen.core-test> (disp-macro #{1 2 3 4})
;; x =  #{1 4 3 2}
;; #{1 4 3 2}
;; malen.core-test> 

;; malen.core-test> #{(println 1) (println 2) (println 3)}
;; 3
;; 1
;; 2
