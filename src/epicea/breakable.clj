(ns epicea.breakable)

;; To think about:
;;  * Don't change the order in which functions are executed
;;  * Don't reorder variables with same name (avoid shadowing). When in doubt, rebind to
;;    a gensym.


(declare build)

(def ^:dynamic rewrite-recur false)

(defn builder [otherwise]
  (fn [y] (build y otherwise)))

(defn form-of-type? [types x]
  (and (seq? x) (contains? types (first x))))

(defn primitive? [x]
  (not (coll? x)))

(defn special-call? [x]
  (contains? #{'throw 'throw*} x))

(defn rebind? [x]
  (or (not (primitive? x))
      (and (symbol? x) (nil? (resolve x)) (not (special-call? x)))))

(defn fixed-value? [x]
  (not (rebind? x)))

(defn to-fun [value]
  (if (fn? value) 
    value
    (fn [cb]
      (cb value))))

;; Use this when you want to call a callback
;; Instead of doing (cb x), do (call-cb cb x)
(defn call-cb [cb value0]
  ((to-fun value0) 
   (fn [value]
     (if (primitive? value) ;; fixed-value?
       (cb value)
       (let [sym (gensym)]
         `(let [~sym ~value]
            ~(cb sym)))))))

;; Use this when you return something which does not need a callback
(defn make-result [x]
  (if (or (fn? x) (primitive? x)) ; fixed-value?
    x
    (let [sym (gensym)]
      (fn [cb]
        `(let [~sym ~x]
           ~(cb sym))))))


(defn make-subexpr [x otherwise]
  (make-result (build x otherwise)))


(defn expect [& args ] 
  (println "EXPECT CALLED, NOT COVERED BY BREAKABLE")
  (assert false))

(defn cancel [& args]
  (println "CANCEL CALLED, NOT COVERED BY BREAKABLE")
  (assert false))

(defn coll-to-seq [x]
  (if (coll? x) (seq x) x))
    
(defn contains-recursive? [x0 eset]
  (let [x (coll-to-seq x0)]
    (or (if (coll? x) (some (fn [y] (contains-recursive? y eset)) x))
        (contains? eset x))))

(defn contains-expect? [x]
  (when (contains-recursive? x #{'malen.core/expect})
    (println "expect is forbidden in subtree " (str x) " but that is violated")
    true))

(defn check-no-expect [x]
  (assert (not (contains-expect? x)))
  x)

(defn build-if [x otherwise]
  (let [parsed (map (builder otherwise) (rest x))
        [test on-true on-false] parsed]
    (if (some fn? parsed)
      (fn [cb]
         ((to-fun test)
          (fn [outcome]
            (if (some fn? [on-true on-false])
              `(if ~outcome
                 ~((to-fun on-true) cb)
                 ~((to-fun on-false) cb))
              (call-cb cb `(if ~outcome ~on-true ~on-false))))))
      `(if ~@parsed))))

    
(defn make-pairs [x0]
  (loop [data x0
         result []]
    (if (empty? data)
      result
      (let [[a b & rest-data] data]
        (recur rest-data (conj result [a b]))))))


(defn let? [b]
  (form-of-type? #{'let 'let* 'clojure.core/let} b))

(defn do? [x]
  (form-of-type? #{'do 'do*} x))


(defn make-let [a-sym a-expr b]
  (cond
    (let? b)
    (let [[l b-bindings & b-body] b]
      `(let [~a-sym ~a-expr ~@b-bindings]
         ~@b-body))

    (do? b)
    (let [[b-do & b-body] b]
      `(let [~a-sym ~a-expr] ~@b-body)) 
    
    :else
    `(let [~a-sym ~a-expr]
       ~b)))

(defn build-let-sub [bindings end-form]
  (if (empty? bindings)
    end-form
    (let [[[b-sym b-expr] & rest-b] bindings]
      ((to-fun b-expr) 
       (fn [b-value]
         (make-let 
          b-sym b-value 
          (build-let-sub rest-b end-form)))))))

(defn binding-builder [otherwise]
  (fn [[vars expr]]
    [vars (build expr otherwise)]))

(defn build-bindings [bindings0 otherwise]
  (map (binding-builder otherwise) (make-pairs bindings0)))

(defn build-many [body otherwise]
  (build `(do ~@body) otherwise))

(defn build-let [x otherwise]
  (let [[l bindings0 & body] x
        bindings (build-bindings bindings0 otherwise)
        bb (build-many body otherwise)]
    (cond

      (some fn? (map second bindings))
      (fn [cb]
        (build-let-sub 
         bindings
         ((to-fun (make-result bb)) cb)))

      (fn? bb)
      (fn [cb]
        `(let ~bindings0 ~((to-fun bb) cb)))

      :else `(let ~bindings0 ~bb))))


(defn make-loop-bindings [bindings] 
  (reduce
   concat
   (map (fn [[a b]]
         [a a])
        bindings)))

(defn make-syms [n]
  (for [i (range n)]
    (gensym)))


(defn make-otherwise-expr [evec sym]
  (assert (vector? evec))
  (assert (not (fn? sym)))
  (if (empty? evec)
    sym
    (let [e (first evec)]
      (cond
        (or (number? e) (keyword? e) (nil? e) (string? e)) e
        :else `(if (fn? ~e)
                 (~e ~sym)
                 ~e)))))



(defn build-loop-body-rewrite-recur [body]
  (binding [rewrite-recur true]
    ((to-fun (build-many body
                   `(fn [x#] [:otherwise x#])))
            (fn [x] [:done x]))))

(defn make-rewritten-loop [body bindings ret]
  `(loop ~(vec (make-loop-bindings bindings))
     (let [~ret ~(build-loop-body-rewrite-recur body)]
       (if (= :recur (first ~ret))
         ~(let [syms (make-syms (count bindings))]
            `(let [~(vec syms) (rest ~ret)]
               (recur ~@syms)))
         ~ret))))

(defn build-loop-sub-rewritten-recur [bindings body otherwise]
  (let [ret (gensym)
        final-ret (gensym)
        value (gensym)]
    (fn [cb]
      (build-let-sub
       bindings
       `(let [~final-ret ~(make-rewritten-loop body bindings ret)
              ~value (second ~final-ret)]
          (if (= (first ~final-ret) :done)
            ~(cb value)
            ~(make-otherwise-expr otherwise value)))))))
     
(defn build-loop-sub [bindings body otherwise]
  (let [loop-body (binding [rewrite-recur false]
                    (build-many body otherwise))]
    (if (fn? loop-body)
      (build-loop-sub-rewritten-recur bindings body otherwise)
      `(loop ~(vec (make-loop-bindings bindings))
         ~loop-body))))

(defn build-loop [x otherwise]
  (let [[l bindings0 & body] x
        bindings (build-bindings bindings0 otherwise)]
    (build-loop-sub bindings body otherwise)))
    


(defn build-final-coll [dst pairwise? data]
  (into dst (if pairwise? (make-pairs data) data)))

(defn build-coll-generic [dst args symbols pairwise? cb]
  (if (empty? args)
    (call-cb cb (build-final-coll dst pairwise? symbols))
    ((to-fun (first args))
     (fn [value]
       (if (fixed-value? value) ;fixed-value? MUST be here
         (build-coll-generic dst (rest args) (conj symbols value) pairwise? cb)
         (let [sym (gensym)]
           (make-let sym value 
                     (build-coll-generic dst (rest args)
                                         (conj symbols sym) 
                                         pairwise?
                                         cb))))))))    


(defn build-coll-sub [dst x otherwise pairwise?]
  (let [args (map (builder otherwise) 
                  (if pairwise? 
                    (reduce concat (vec x)) x))]
    (if (some fn? args)
      (fn [cb]
        (build-coll-generic dst (map make-result args) [] pairwise? cb))
      (build-final-coll dst pairwise? args))))

(defn build-map [x otherwise]
  (build-coll-sub {} x otherwise true))

(defn build-vec [x otherwise]
  (build-coll-sub [] x otherwise false))

(defn build-set [x otherwise]
  (build-coll-sub #{} x otherwise false))


(defn read-while [pred? x0]
  (loop [x x0
         result []]
    (if (empty? x)
      [result x]
      (let [f (first x)]
        (if (pred? f)
          (recur (rest x) (conj result f))
          [result x])))))

(defn parse-try [x]
  (let [[t & body] x]
    (let [[exprs rem] 
          (read-while 
           (fn [y] (not (form-of-type? #{'catch 'catch* 'finally 'finally*} y)))
           body)
          [c finally-clause] (read-while (fn [y] (form-of-type? #{'catch* 'catch} y)) rem)]
      (assert (<= (count finally-clause) 1))
      [exprs c (first finally-clause)])))

(defn build-catch [x otherwise]
  (let [[c t v & body] x]
    `(catch ~t ~v ~((to-fun (build-many body otherwise)) identity))))

(defn build-finally [x otherwise]
  (let [[f & body] x]
    `(finally ~((to-fun (build-many body otherwise)) identity))))



(defn build-try [x otherwise]
  (let [[exprs catches fin] (parse-try x)]
    `(try
       ~((to-fun (build-many exprs otherwise)) identity)
       ~@(map 
          (fn [x] (build-catch x otherwise))
          catches)
       ~(build-finally fin otherwise))))
    



(defn destructure-expect [x otherwise]
  (assert (contains? #{3 4} (count x)))
  (if (= 3 (count x))
    (let [[e a b] x]
      {:pred? (build a otherwise)
       :expr (build b otherwise)
       :otherwise []})
    (let [[e a b c] x]
      {:pred? (build a otherwise)
       :expr (build b otherwise)
       :otherwise [c]})))




(defn make-expect-if [pred? sym inner-otherwise otherwise cb]
  `(if (~pred? ~sym)
     ~(call-cb cb sym)
     ~(make-otherwise-expr
       otherwise
       (make-otherwise-expr inner-otherwise sym))))

(defn build-expect [x otherwise]
  (assert (vector? otherwise))
  (let [e (destructure-expect x otherwise)
        sym (gensym)]
    (fn [cb]  ;; This is the one returned
      ((to-fun (:pred? e)) 
       (fn [pred?]
         ((to-fun (:expr e))
          (fn [value]
            (if (coll? value)
              `(let [~sym ~value]
                 ~(make-expect-if pred? sym (:otherwise e) otherwise cb))
              (make-expect-if pred? value (:otherwise e) otherwise cb)))))))))


(defn build-cancel [x otherwise]
  (let [sym (gensym)]
    (fn [cb]
      `(let [~sym ~(second x)]
         ~(make-otherwise-expr otherwise sym)))))


(defmacro debug-build [x]
  ((to-fun (build x [:otherwise-branch])) identity))
  


;; Returns an expression,
;; given mapped args, a cumulative list of symbols,
;; and a callback that will consume the final expression
;;
;; Possible optimization: Once there are only regular expressions
;; left, we can insert them directly
(defn build-fun-call-sub [args symbols cb]
  (if (empty? args)
    (call-cb cb (seq symbols)) ;;; <-- OK??? Yes, think so
    (let [f (first args)]
      (cond
        (fn? f) (f (fn [result]
                     (build-fun-call-sub 
                      (rest args) 
                      (conj symbols result)
                      cb)))
        (fixed-value? f) ; fixed-value? MUST be here
        (build-fun-call-sub (rest args)
                            (conj symbols f)
                            cb)
        :else 
        (let [sym (gensym)]
          `(let [~sym ~f]
             ~(build-fun-call-sub (rest args) 
                                  (conj symbols sym)
                                  cb)))))))

; (build-fun-call-sub '(+ 2 3) [] (fn [x] `(* 2 ~x)))
; (build-fun-call-sub '(+ (+ 2 4) 3) [] (fn [x] `(* 2 ~x)))
; (build-fun-call-sub `(+ 2 ~(fn [cb] (cb 34))) [] (fn [x] `(* 2 ~x)))

(defn build-fun-call [x otherwise]
  (if (empty? x)
    x
    (let [args (map (builder otherwise) x)]
      (if (some fn? args)
        (fn [cb] (build-fun-call-sub (map make-result args) [] cb))
        args))))

(defn make-do [a b]
  (if (do? b)
    `(do ~a ~@(rest b))
    `(do ~a ~b)))

(defn build-do-sub [args cb]
  (if (empty? args)
    nil
    (let [f (first args)
          r (rest args)]
      (if (fn? f)
        (f (fn [value]
             (if (empty? r)
               (call-cb cb value)
               (build-do-sub r cb))))
        (if (empty? r)
          (call-cb cb f)
          (make-do f (build-do-sub r cb)))))))


(defn build-do [x otherwise]
  (let [args (map (builder otherwise) (rest x))]
    (if (some fn? args)
      (fn [cb]
        (build-do-sub (map make-result args) cb))
      `(do ~@args))))

(defn build-recur-sub [args syms]
  (if (empty? args)
    `[:recur ~@syms]
    ((to-fun (first args))
     (fn [sym]
       (build-recur-sub (rest args) (conj syms sym))))))

(defn build-recur-OLD [x otherwise]
  (let [args (map (builder otherwise) (rest x))]
    (if (some fn? args)
      (fn [cb]
        (build-recur-sub args []))
      (if rewrite-recur
        (fn [cb]
          `[:recur ~@args])
        `(recur ~@args)))))

(defn rebind-arg-list 
  ([args syms cb]
   (if (empty? args) 
     (cb syms)
     ((to-fun (first args))
      (fn [sym]
        (rebind-arg-list (rest args) 
                         (conj syms sym) 
                         cb)))))
  ([args cb]
   (rebind-arg-list args [] cb)))
 


(defn build-recur [x otherwise]
  (fn [cb]
    (rebind-arg-list
     (map (builder otherwise) (rest x))
     (fn [syms]
       `(recur ~@syms)))))


(defn build-seq [x otherwise] 
  (if-let [f (case (first x)
               quote nil
               fn* nil
               fn nil
               do* build-do
               do build-do
               if* build-if
               if build-if
               let* build-let
               let build-let

               ;; Exception handling
               try* build-try
               try build-try
               catch nil;
               catch* nil
               finally nil
               finally* nil

               expect build-expect
               cancel build-cancel

               ;; Explicitly forbid recur and loop because the generated code
               ;; is way too ugly.
               ;;
               recur build-recur
               recur* build-recur
               loop nil ;build-loop
               loop* nil ;build-loop

               ;; Throw is just like a function call

               build-fun-call)]
    (f x otherwise) 
    (check-no-expect x)))

   
(defn build-coll [x otherwise]
  ((cond
     (map? x) build-map
     (seq? x) build-seq
     (vector? x) build-vec
     (set? x) build-set
     :default :unhandled) x otherwise))

;; Build either returns
;;  * A regular value that is not a function
;;  * A function, that accepts a callback.
;;    * The callback is called with the resulting expression
(defn build [x otherwise]
  (assert (vector? otherwise))
  (if (primitive? x)
    x
    (build-coll x otherwise)))

(defn split-forms [forms]
  (let [n (count forms)]
    (if (and (<= 2 n) (= :otherwise (nth forms (- n 2))))
      [(subvec forms 0 (- n 2)) [(last forms)]]
      [forms []])))

(defn with-rebound-otherwise [otherwise cb]
  (if (empty? otherwise)
    (cb [])
    (let [e (first otherwise)]
      (if (rebind? e)
        (let [o-sym (gensym)]
          `(let [~o-sym ~e]
             ~(cb [o-sym])))
        (cb otherwise)))))

(defmacro breakable [& forms]
  (let [[body otherwise] (split-forms (vec forms))]
    (with-rebound-otherwise otherwise
      (fn [o-sym]
        ((to-fun (build (clojure.walk/macroexpand-all`(do ~@body)) o-sym)) identity)))))
