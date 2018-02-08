(ns psiml.type-test
  (:require [clojure.test :refer :all]
            [psiml.type :refer :all]))

(def t-int [:t :int])
(def t-bool [:t :bool])

(deftest test-free-vars
  (is (= (free-vars [:var :x] #{}) #{:x}))
  (is (= (free-vars [:var :x] #{:x}) #{}))
  (is (= (free-vars [:t-abs :x [:var :x]] #{}) #{}))
  (is (= (free-vars
           [:t-abs :x [:abs [:var :x] [:var :y]]] #{})
         #{:y})))

(def s-iibb [:struct {:i t-int :b t-bool}])
(def s-ibbi [:struct {:i t-bool :b t-int}])
(def s-abiibb [:struct {:a t-bool :i t-int :b t-bool}])

(defn is-meet?
  [a b r]
  (is (= (simplify-input [:meet a b]) r)))

(deftest test-meet
  (is-meet? t-int t-int t-int)
  (is-meet? t-int t-bool [:meet t-bool t-int])
  (is-meet? s-iibb s-iibb s-iibb)
  (is-meet? s-iibb s-ibbi
            [:struct {:i [:meet t-bool t-int]
                      :b [:meet t-bool t-int]}])
  (is-meet? s-abiibb s-iibb s-abiibb))

(defn is-join?
  [a b r]
  (is (= (simplify-output [:join a b]) r)))

(deftest test-join
  (is-join? t-int t-int t-int)
  (is-join? t-int t-bool [:join t-bool t-int])
  (is-join? s-iibb s-iibb s-iibb)
  (is-join? s-iibb s-ibbi
            [:struct {:i [:join t-bool t-int]
                      :b [:join t-bool t-int]}])
  (is-join? s-abiibb s-iibb s-iibb))

(defn is-biunify?
  [a b r]
  (is (= (first (biunify [[a b]] a {})) r)))

(deftest test-biunify
  (is-biunify? t-int t-int t-int)
  (is-biunify? t-int t-bool nil)
  (is-biunify? s-iibb s-iibb s-iibb)
  (is-biunify? s-iibb s-ibbi nil)
  (is-biunify? s-abiibb s-iibb s-abiibb)
  (is-biunify? s-iibb s-abiibb nil))

(deftest test-eq?
  (is (eq? [:t-abs :a [:var :a]]
           [:t-abs :b [:var :b]])))

(defn is-typed?
  ([e t] (is-typed? e t {}))
  ([e t vars]
   (let [[t' _] (expr e vars)]
     (is (eq? t' t)))))

(deftest test-expr
  (is-typed? [:lit 1] t-int)
  (is-typed? [:lit true] t-bool)
  (let [s [:struct {:a [:lit 1] :b [:lit true]}]]
    (is-typed? s [:struct {:a t-int :b t-bool}])
    (is-typed? [:get :a s] t-int)))

(def id [:abs :x [:var :x]])

(deftest expr-id
  (is-typed? id [:abs [:var :a] [:var :a]]))

(deftest select-int-bool
  (is-typed? [:abs :p
              [:abs :v
               [:abs :d
                [:if [:app [:var :p] [:var :v]]
                 [:app [:var :int-id] [:var :v]]
                 [:app [:var :bool-id] [:var :d]]]]]]
             [:abs [:abs t-int t-bool]
              [:abs t-int
               [:abs t-bool
                [:join t-bool t-int]]]]
             {:int-id [:abs t-int t-int]
              :bool-id [:abs t-bool t-bool]}))

(deftest int-or-bool
  (is-typed? [:if [:var :t] [:lit 1] [:lit true]]
             [:join [:t :bool] [:t :int]]))

(deftest a-or-b
  (is-typed? [:if [:var :t] [:var :a] [:var :b]]
             [:join [:var :a] [:var :b]]))

(deftest select
  (is-typed? [:abs :p
              [:abs :v
               [:abs :d
                [:if [:app [:var :p] [:var :v]]
                 [:var :v]
                 [:var :d]]]]]
             [:abs [:abs [:var :v] t-bool]
              [:abs [:var :v]
               [:abs [:var :d]
                [:join [:var :v] [:var :d]]]]]))

;; NEXT TARGET:
;; (deftest meet-and
;;   (is-typed? [:abs :p
;;               [:abs :a
;;                [:abs :b
;;                 [:if [:app [:var :p] [:var :a]]
;;                  [:app [:var :p] [:var :b]]
;;                  [:lit false]]]]]
;;              [:t-abs :a
;;               [:t-abs :b
;;                [:abs [:abs [:meet [:var :a] [:var :b]] t-bool]
;;                 [:abs [:var :a]
;;                  [:abs [:var :b]
;;                   t-bool]]]]]))
