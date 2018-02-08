(ns psiml.print
  (:require [clojure.core.match :refer [match]])
  (:require [psiml.util :refer [flatten-kw interpose-fn]]))

(defn expr
  [e]
  (match e
    [:abs n e1] (do (print "λ") (print n) (print ".") (expr e1))
    [:app e1 e2] (do (print "(") (expr e1)
                     (print " ") (expr e2) (print ")"))
    [:lit l] (print l)
    [:var n] (print n)
    [:struct m]
    (do (print "{")
        (interpose-fn #(print " ")
                      (map (fn [[l e]]
                             #(do (print l)
                                  (print " ")
                                  (expr e))) m))
        (print "}"))
    [:get l e1] (do (print "(") (print l)
                    (print " ") (expr e1) (print ")"))
    [:if test then else]
    (do (print "(if ") (expr test)
        (print " then ") (expr then)
        (print " else ") (expr else) (print ")"))))

(defn type
  [t]
  (match t
    [:top] (print "⊤")
    [:bot] (print "⊥")
    [:t base] (print base)
    [:var n] (do (print "'") (print n))
    [:abs t1 t2] (do (print "((") (type t1) (print ")")
                     (print " → ") (type t2) (print ")"))
    [:struct m]
    (do (print "{")
        (interpose-fn #(print " ")
                      (map (fn [[l t]]
                             #(do (print l)
                                  (print " ")
                                  (type t))) m))
        (print "}"))
    [:meet & ts]
    (interpose-fn #(print " ⊓ ")
                  (map #(fn [] (type %))
                       (flatten-kw :meet ts [])))
    [:join & ts]
    (interpose-fn #(print " ⊔ ")
                  (map #(fn [] (type %))
                       (flatten-kw :join ts [])))))
