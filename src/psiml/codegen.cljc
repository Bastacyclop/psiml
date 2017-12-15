(ns psiml.codegen
  (:require [clojure.core.match :refer [match]]))

(declare expr)

(defn fields-to-json
  [[field value & rest]]
  (str "'" (name field) "': " (expr value)
       (if (not (empty? rest))
         (str ", " (fields-to-json rest)))))

(defn struct-to-json-string
  "Given a struct type, returns its stringified json representation."
  [struct]
  (str "{" (if (not (empty? struct)) (fields-to-json struct)) "}"))

(defn expr
  "Generates javascript code.
  ast: A typed AST
  returns: A string containing js code"
  [ast]
  (match ast
         {:node [:lit :true] :type [:bool]} "true"
         {:node [:lit :false] :type [:bool]} "false"
         {:node [:lit x] :type [:int]} (str x)
         {:node [:lit x] :type [:struct]} (struct-to-json-string x)
         _ "error"
     ))
