(ns psiml.codegen-test
  (:require [clojure.test :refer :all]
            [psiml.codegen :refer :all]))

(deftest literals
  (testing "Generating code for literals"
    (is (= "true" (expr {:node [:lit :true] :type [:bool]})))
    (is (= "false" (expr {:node [:lit :false] :type [:bool]})))
    (is (= "42" (expr {:node [:lit 42] :type [:int]})))
    (is (= "-42" (expr {:node [:lit -42] :type [:int]})))
    (is (= "{}" (expr {:node [:lit []] :type [:struct]})))
    (is (= "{'one': 1, 'true': true, 'obj': {}}"
           (expr {:node [:lit [:one {:node [:lit 1] :type [:int]} 
                               :true {:node [:lit :true] :type [:bool]}
                               :obj {:node [:lit []] :type [:struct]} ]]
                  :type [:struct]})))))
