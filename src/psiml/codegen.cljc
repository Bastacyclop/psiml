(ns psiml.codegen
  (:require [clojure.core.match :refer [match]]))

#?(:cljs (enable-console-print!))

(declare codegen-expr)

(defn fields-to-json
  [[field expr & rest]]
  (str "'" (name field) "': " (codegen-expr expr)
       (if (not (empty? rest))
         (str ", " (fields-to-json rest)))))

(defn struct-to-json-string
  "Given a struct type, returns its stringified json representation."
  [struct]
  (str "{" (if (not (empty? struct)) (fields-to-json struct)) "}"))

(defn codegen-expr
  "Generates javascript code.
  ast: A typed AST
  returns: A string containing js code"
  [ast]
  (println "codegen-expr:" ast)
  (match ast
         {:node [:const :true] :type [:bool]} "true"
         {:node [:const :false] :type [:bool]} "false"
         {:node [:const x] :type [:int]} (str x)
         {:node [:const x] :type [:struct]} (struct-to-json-string x)
         _ "error"
     ))
