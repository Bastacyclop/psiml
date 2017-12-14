(ns psiml.codegen-test
  (:require [clojure.test :refer :all]
            [psiml.codegen :refer :all]))

(deftest codegen-const
  (testing "Generating code for constants"
    (is (= "true" (codegen-expr {:node [:const :true] :type [:bool]})))
    (is (= "false" (codegen-expr {:node [:const :false] :type [:bool]})))
    (is (= "42" (codegen-expr {:node [:const 42] :type [:int]})))
    (is (= "-42" (codegen-expr {:node [:const -42] :type [:int]})))
    (is (= "{}" (codegen-expr {:node [:const []] :type [:struct]})))
    (is (= "{'one': 1, 'true': true}" (codegen-expr {:node [:const
                                                            [:one {:node [:const 1] :type [:int]} 
                                                             :true {:node [:const :true] :type [:bool]}]]
                                                     :type [:struct]})))))

; (deftest codegen-var
;   (testing "Generating code for var"
;     (is (=  "variable" (codegen-expr {:node [:var :variable] :type [:bool]})))))

