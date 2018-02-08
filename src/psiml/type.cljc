(ns psiml.type
  "Types abstract syntax, producing typed abstract syntax"
  #?(:cljs (:require-macros psiml.type))
  (:require [clojure.core.match :refer [match]]
            [psiml.util :refer [debug flatten-kw merge-kw]]
            [psiml.print :as prt]))

;; TODO: typed ast {:node n :type t}

;; type environment: {name -> input type}

(defn print-t-env
  [[t env]]
  (do (print "type: ") (prt/type t) (println)
      (print "env: ") (prt/type [:struct env]) (println)))

(declare bisubstitute-input)
(declare bisubstitute-output)

(defn bisubstitute-input
  "Substitutes input occurences of n with t-in and
  output occurences with t-out in the input type t."
  [n t-in t-out t]
  (let [sub-in #(bisubstitute-input n t-in t-out %)
        sub-out #(bisubstitute-output n t-in t-out %)]
    (match t
      [:var n'] (if (= n n') t-in [:var n'])
      [:struct t-m]
      [:struct (into {} (map (fn [[l t]] [l (sub-in t)]) t-m))]
      [:abs t1 t2] [:abs (sub-out t1) (sub-in t2)]
      ;; [:rec t] [:rec (sub-in t)]
      [:meet & ts] (into [:meet] (map sub-in ts))
      :else t)))

(defn bisubstitute-output
  "Substitutes input occurences of n with t-in and
  output occurences with t-out in the output type t."
  [n t-in t-out t]
  (let [sub-in #(bisubstitute-input n t-in t-out %)
        sub-out #(bisubstitute-output n t-in t-out %)]
    (match t
      [:var n'] (if (= n n') t-out [:var n'])
      [:struct t-m]
      [:struct (into {} (map (fn [[l t]] [l (sub-out t)]) t-m))]
      [:abs t1 t2] [:abs (sub-in t1) (sub-out t2)]
      ;; [:rec t] [:rec (sub-out t)]
      [:join & ts] (into [:join] (map sub-out ts))
      :else t)))

(defn bisubstitute-env
  "Substitutes input occurences of n with t-in and
  output occurences with t-out in the environment."
  [n t-in t-out env]
  (let [sub-in #(bisubstitute-input n t-in t-out %)]
    (->> env
         (map (fn [[n' t']] [n' (sub-in t')]))
         (into {}))))

(defn bisubstitute-constraints
  [n t-in t-out constraints]
  (into []
        (map (fn [[to ti]] [(bisubstitute-output n t-in t-out to)
                            (bisubstitute-input n t-in t-out ti)])
             constraints)))

(assoc-in {:x {:y 1}} [:x :y] 2)

(defn take-var-t
  "Removes the type t of the variable n from env
   and return [t env]."
  [n env] [(or (env n) [:top]) (dissoc env n)])

(defn free-vars
  ([t bound] (free-vars t bound #{}))
  ([t bound res]
   (let [collect #(reduce (fn [r t] (free-vars t bound r))
                          res %)]
     (match t
       [:top] res
       [:bot] res
      [:t _] res
       [:var n] (if (bound n) res (conj res n))
       [:struct t-m] (collect (vals t-m))
       [:abs a b] (collect [a b])
       [:t-abs n t'] (free-vars t' (conj bound n) res)
       [:meet & ts] (collect ts)
       [:join & ts] (collect ts)
       :else res))))

;; FIXME: simplification
(declare simplify-input)
(declare simplify-output)

(defn simplify-input
  [t]
  (match t
    [:abs to ti]
    [:abs (simplify-output to) (simplify-input ti)]
    [:struct m]
    [:struct
     (into {} (map (fn [[l ti]] [l (simplify-input ti)]) m))]
    [:meet & ts]
    (let [meets (flatten-kw :meet ts [])
          meets (group-by #(first %) meets)
          var (into #{} (:var meets))
          basic (into #{} (:t meets))
          abs (if-let [as (:abs meets)]
                [(simplify-input
                   [:abs
                    (merge-kw :join (map (fn [[_ to _]] to) as))
                    (merge-kw :meet (map (fn [[_ _ ti]] ti) as))])])
          struct (if-let [ss (:struct meets)]
                   [(simplify-input
                      [:struct
                       (apply merge-with
                              (fn [a b] [:meet a b])
                              (map (fn [[_ m]] m) ss))])])]
      (or (merge-kw :meet (concat var basic abs struct)) [:top]))
    :else t))

(defn simplify-output
  [t]
  (match t
    [:abs ti to]
    [:abs (simplify-input ti) (simplify-output to)]
    [:struct m]
    [:struct
     (into {} (map (fn [[l to]] [l (simplify-output to)]) m))]
    [:join & ts]
    (let [joins (flatten-kw :join ts [])
          joins (group-by #(first %) joins)
          var (into #{} (:var joins))
          basic (into #{} (:t joins))
          abs (if-let [as (:abs joins)]
                [(simplify-output
                   [:abs
                    (merge-kw :meet (map (fn [[_ ti _]] ti) as))
                    (merge-kw :join (map (fn [[_ _ to]] to) as))])])
          struct (if-let [ss (:struct joins)]
                   (let [ms (map (fn [[_ m]] m) ss)
                         ls (apply clojure.set/intersection
                                   (map #(into #{} (keys %)) ms))]
                     [(simplify-output
                        [:struct
                         (apply merge-with
                                (fn [a b] [:join a b])
                                (map #(into {}
                                            (filter
                                              (fn [[l _]] (ls l))
                                              %))
                                     ms))])]))]
      (or (merge-kw :join (concat var basic abs struct)) [:bot]))
    :else t))

(defn simplify
  [[t env]]
  [(simplify-output t)
   (->> env
        (map (fn [[n t]] [n (simplify-input t)]))
        (into {}))])

(defn biunify
  "Biunifies each constraint, for the output to flow in the input.
  Applies the bisubstitutions on the output type t and
  the negative environment env, returning [t env]."
  [constraints t env]
  (if-let [[t-out t-in] (peek constraints)]
    (let [cs (pop constraints)
          [cs t env]
          (match [t-out t-in]
            [_ [:top]] [cs t env]
            [[:bot] _] [cs t env]
            [[:t a] [:t b]]
            (if (= a b) [cs t env] [[] nil env])
            ;; TODO [[:var a] [:var a]] (recur cs t env)
            [[:var a] _]
            (let [t-in [:meet [:var a] t-in]
                  cs (bisubstitute-constraints a t-in t-out cs)
                  t (bisubstitute-output a t-in t-out t)
                  env (bisubstitute-env a t-in t-out env)]
              [cs t env])
            [_ [:var a]]
            (let [t-out [:join [:var a] t-out]
                  cs (bisubstitute-constraints a t-in t-out cs)
                  t (bisubstitute-output a t-in t-out t)
                  env (bisubstitute-env a t-in t-out env)]
              [cs t env])
            [_ [:meet & ts]]
            [(into cs (map (fn [t] [t-out t]) ts)) t env]
            [[:join & ts] _]
            [(into cs (map (fn [t] [t t-in]) ts)) t env]
            [[:struct tm-out] [:struct tm-in]]
            [(reduce (fn [cs [l ti]] (conj cs [(tm-out l) ti]))
                           cs tm-in)
                   t env]
            [[:abs toi too] [:abs tio tii]]
            [(into cs [[tio toi] [too tii]]) t env]
            ;; [[:rec to] [:rec ti]]
            :else [[] nil env])]
      (recur cs t env))
      [t env]))

(defn env-meet
  [envs]
  (apply merge-with (fn [a b] [:meet a b]) envs))

(defn expr
  "Types an expression"
  ([e] (expr e {}))
  ([exp env]
   (let [te
     (simplify (match exp
       [:abs n e]
       (if-let [[t-e env] (expr e env)]
         (if-let [[t-n env] (take-var-t n env)]
           [[:abs t-n t-e] env]))
       ;; [:rec n e]
       ;; (with-env env
       ;;   [t-n t-e] (with-var n #(expr e %))
       ;;   t (biunify t-e t-n)
       ;;   [:rec n t])
       [:lit c] [(cond (integer? c) [:t :int]
                       (or (true? c) (false? c)) [:t :bool])
                 env]
       [:var n] (if-let [t (env n)]
                  [t env]
                  (let [t [:var (gensym "t")]]
                    [t (assoc env n t)]))
       [:app e1 e2]
       (if-let [[t1 env1] (expr e1 env)]
         (if-let [[t2 env2] (expr e2 env)]
           (let [t? [:var (gensym "a")]]
             (biunify [[t1 [:abs t2 t?]]]
                      t? (env-meet [env1 env2])))))
       [:struct m]
       (if-let [t-env-m (reduce
                          (fn [t-env-m [l e]]
                               (if-let [t-env (expr e env)]
                                 (assoc t-env-m l t-env)
                                 (reduced nil)))
                          {} m)]
         [[:struct (into {} (map (fn [[l [t _]]] [l t]) t-env-m))]
          (env-meet (map #(second %) (vals t-env-m)))])
       [:get l e]
       (if-let [[t env] (expr e env)]
         (let [t? [:var (gensym "g")]]
           (biunify [[t [:struct {l t?}]]]
                    t? env)))
       [:if e-test e-then e-else]
       (if-let [[t-test env-test] (expr e-test env)]
         (if-let [[t-then env-then] (expr e-then env)]
           (if-let [[t-else env-else] (expr e-else env)]
             (let [t? [:var (gensym "i")]]
               (biunify [[t-test [:t :bool]]
                         [t-then t?]
                         [t-else t?]]
                        t?
                        (env-meet [env-test env-then env-else]))))))
       :else nil))]
     (do (prt/expr exp) (println) (print-t-env te) te))))

(defn eq?
  ([t1 t2] (eq? t1 t2 {}))
  ([t1 t2 vars]
   (let [every-eq? #(reduce (fn [vs [a b]]
                              (or (eq? a b vs) (reduced false)))
                            vars %)]
     (match [t1 t2]
       [[:top] [:top]] vars
       [[:bot] [:bot]] vars
       [[:t b1] [:t b2]] (if (= b1 b2) vars)
       [[:var n1] [:var n2]] (if (contains? vars n1)
                               (if (= (vars n1) n2) vars)
                               (assoc vars n1 n2))
       [[:struct tm1] [:struct tm2]]
       (and (= (into #{} (keys tm1)) (into #{} (keys tm2)))
            (every-eq? (map (fn [[l t1]] [t1 (tm2 l)]) tm1)))
       [[:abs a1 b1] [:abs a2 b2]]
       (every-eq? [[a1 a2] [b1 b2]])
       [[:t-abs n1 t1'] [:t-abs n2 t2']]
       (eq? t1' t2')
       ;; [[:rec a1] [:rec a2]] (eq? [a1 env1] [a2 env2])
       [[:meet & ts1] [:meet & ts2]]
       (every-eq? (map (fn [t1 t2] [t1 t2]) ts1 ts2))
       [[:join & ts1] [:join & ts2]]
       (every-eq? (map (fn [t1 t2] [t1 t2]) ts1 ts2))
       :else false))))
