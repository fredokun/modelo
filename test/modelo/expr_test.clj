(ns modelo.expr-test
  (:require [modelo.expr :as e]
            [clojure.test :as t :refer [use-fixtures deftest is]]))

;; (t/deftest blabla)

(defn parser-fixture [f]
  (e/register-var-parselet!)
  (f)
  (e/clear-parselets!))


(use-fixtures :once parser-fixture)


(deftest parse-var
  (is (= (:name (e/check-parse-expr 'x)) 'x))
  (is (= (:kind (e/check-parse-expr 'x)) :unspecified)))





