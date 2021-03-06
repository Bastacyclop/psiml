(ns psiml.type
  "Types abstract syntax, producing typed abstract syntax"
  #?(:cljs (:require-macros psiml.type))
  (:require [clojure.core.match :refer [match]]))

;; TODO: typed ast {:node n :type t}

;; type environment: {:vars {name -> input type}
;;                    :bound-t-vars #{name}}

(defmacro trace
  [sym env]
  `(do (println ~(str sym) ~sym ~env) [~sym ~env]))

(defn map-env
  "Returns (f t env) if t is not nil, otherwise [t env]."
  [[t env] f]
  (if (nil? t) [t env] (f t env)))

(defmacro with-env
  "Passes an environment through a series of operations,
  binding the results and returning a final value.

  The operations are given the environment as an additional
  argument and should return a value of the form [result new-env].
  The binding itself is done using map-env, thus aborting the
  series of operations after an error."
  ([env r] `[~r ~env])
  ([env b f & binds]
   (let [senv (if (symbol? env) env (gensym))]
     `(map-env (->> ~env ~f)
               (fn [~b ~senv] (with-env ~senv ~@binds))))))

(defn env-id
  "Returns v without affecting the environment."
  [v env]
  [v env])

(defn env-only
  "Applies f to the environment, returning a dummy value."
  [f env]
  [:ok (f env)])

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
      [:meet t1 t2] [:meet (sub-in t1) (sub-in t2)]
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
      [:join t1 t2] [:join (sub-out t1) (sub-out t2)]
      :else t)))

(defn bisubstitute-env
  "Substitutes input occurences of n with t-in and
  output occurences with t-out in the environment."
  [n t-in t-out env]
  (let [sub-in #(bisubstitute-input n t-in t-out %)]
    (update env :vars
            #(->> %
                  (map (fn [[n' t']] [n' (sub-in t')]))
                  (into {})))))

(defn replace-var-t
  "Replaces the type of the variable n with t,
  simply removes the variable if t is nil.
  Returns the replaced type."
  [n t env]
  (let [t-n ((:vars env) n)]
    [t-n (update env :vars #(if t (assoc % n t) (dissoc % n)))]))

(defn with-var
  "Introduces a new local variable n for the scope of f.
  The result is of the form [[t-n t-f] env]."
  [n f env]
  (let [shadow (env n)]
    (with-env env
      t-f (#(f (assoc-in % [:vars n] [:var (gensym n)])))
      t-n (replace-var-t n shadow)
      [t-n t-f])))

(defn new-t-var
  [p f env]
  (let [n (gensym p)]
    (f [:var n] env)))

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
       [:meet a b] (collect [a b])
       [:join a b] (collect [a b])))))

(defn abstract-types ; TODO: polymorphism
  "Abstracts free type variables of t"
  [t env]
  (let [nb (free-vars t (:bound-t-vars env))]
    [(reduce (fn [t n] [:t-abs n t]) t nb) env]))

(declare meet)
(declare join)

(defn meet
  [t1 t2 env]
  (match [t1 t2]
    [[:top] t] [t env]
    [t [:top]] [t env]
    [[:t b1] [:t b2]]
    [(if (= b1 b2) [:t b1] [:meet [:t b1] [:t b2]]) env]
    [[:struct tm1] [:struct tm2]]
    (with-env env
      t-m (#(reduce
              (partial reduce (fn [[t-m env] [l t]]
                                (if (contains? t-m l)
                                  (with-env env
                                    t (meet (t-m l) t)
                                    (assoc t-m l t))
                                  [(assoc t-m l t) env])))
              [{} %]
              [tm1 tm2]))
      [:struct t-m])
    [[:abs t1-out t1-in] [:abs t2-out t2-in]]
    (with-env env
      t-in (meet t1-in t2-in)
      t-out (join t1-out t2-out)
      [:abs t-out t-in])
    ;; [[:rec t1'] [:rec t2']]
    ;; (with-env env
    ;;   t (meet t1' t2')
    ;;  [:rec t])
    :else [[:meet t1 t2] env]))

(defn join
  [t1 t2 env]
  (match [t1 t2]
    [[:bot] t] [t env]
    [t [:bot]] [t env]
    [[:t b1] [:t b2]]
    [(if (= b1 b2) [:t b1] [:join [:t b1] [:t b2]]) env]
    [[:struct tm1] [:struct tm2]]
    (with-env env
      t-m (#(reduce (fn [[t-m env] [l t1]]
                      (if-let [t2 (tm2 l)]
                        (with-env env
                          t (join t1 t2)
                          (assoc t-m l t))
                        [t-m env]))
                    [{} %] tm1))
      [:struct t-m])
    [[:abs t1-in t1-out] [:abs t2-in t2-out]]
    (with-env env
      t-in (meet t1-in t2-in)
      t-out (join t1-out t2-out)
      [:abs t-in t-out])
    ;; [[:rec t1'] [:rec t2']]
    ;; (with-env env
    ;;   t (unify-output t1' t2')
    ;;  [:rec t])
    :else [[:join t1 t2] env]))

(defn biunify
  "Biunifies t-out with t-in in the given environment.
  We want the output to flow in the input.
  Returns [[t-out' t-in'] env]."
  [t-out t-in env]
  (match [t-out t-in]
    [t [:top]] [[t [:top]] env]
    [[:bot] t] [[[:bot] t] env]
    [_ [:var n]] ; TODO
    (with-env env
      _ (env-only #(bisubstitute-env n t-out t-out %))
      [t-out t-out])
    [[:var n] _] ; TODO
    (with-env env
      _ (env-only #(bisubstitute-env n t-in t-in %))
      [t-in t-in])
    [[:t a] [:t b]]
    [(if (= a b) [[:t a] [:t b]]) env]
    [[:struct tm-out] [:struct tm-in]]
    (with-env env
      [mo mi] (#(reduce (fn [[ms env] [l t-in]]
                          (with-env env
                            [mo mi] (env-id ms)
                            t-out (env-id (tm-out l))
                            [to ti] (biunify t-out t-in)
                            [(assoc mo l to) (assoc mi l ti)]))
                    [[{} {}] %]
                    tm-in))
      [[:struct mo] [:struct mi]])
    [[:abs t-out-in t-out-out] [:abs t-in-out t-in-in]]
    (with-env env
      [tio toi] (biunify t-in-out t-out-in)
      [too tii] (biunify t-out-out t-in-in)
      [[:abs toi too] [:abs tio tii]])
    ;; [[:rec t-in] [:rec t-out]] [nil env]
    [_ [:meet t1-in t2-in]]
    (with-env env
      [to t1i] (biunify t-out t1-in)
      [to t2i] (biunify to t2-in)
      ti (meet t1i t2i)
      [to ti])
    [[:join t1-out t2-out] _]
    (with-env env
      [t1o ti] (biunify t1-out t-in)
      [t2o ti] (biunify t2-out ti)
      to (join t1o t2o)
      [to ti])
    :else [nil env]))

(defn expr
  "Types an expression"
  ([e] (expr e {}))
  ([exp env]
   (match exp
     [:abs n e]
     (with-env env
       [t-n t-e] (with-var n #(expr e %))
       [:abs t-n t-e])
     ;; [:rec n e]
     ;; (with-env env
     ;;   [t-n t-e] (with-var n #(expr e %))
     ;;   t (biunify t-e t-n)
     ;;   [:rec n t])
     [:lit c] [(cond (integer? c) [:t :int]
                     (or (true? c) (false? c)) [:t :bool])
               env]
     [:var n] [((:vars env) n) env]
     [:app e1 e2]
     (with-env env
       t1 (expr e1)
       t2 (expr e2)
       [t-abs _] (new-t-var
                   "app"
                   (fn [t? env] (biunify t1 [:abs t2 t?] env)))
       t-out (env-id (match t-abs
                       [:abs _ t-out] t-out))
       t-out)
     [:struct m]
     (with-env env
       t-m (#(reduce (fn [[t-m env] [l e]]
                       (with-env env
                         t (expr e)
                         (assoc t-m l t))) [{} %] m))
       [:struct t-m])
     [:get l e]
     (with-env env
       t (expr e)
       [t-s _] (new-t-var
                 "get"
                 (fn [t? env] (biunify t [:struct {l t?}] env)))
       t-out (env-id (match t-s
                       [:struct {l t-out}] t-out))
       t-out)
     [:if e-test e-then e-else]
     (with-env env
       t-test (expr e-test)
       t-then (expr e-then)
       t-else (expr e-else)
       _ (biunify t-test [:t :bool])
       t (join t-then t-else)
       t)
     :else [nil env])))

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
       [[:meet a1 b1] [:meet a2 b2]]
       (every-eq? [[a1 a2] [b1 b2]])
       [[:join a1 b1] [:join a2 b2]]
       (every-eq? [[a1 a2] [b1 b2]])
       :else false))))
