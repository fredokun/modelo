
(ns modelo.bool
  (:require [clj-by.example :refer [example]])
  (:require [modelo.types :as t])
  (:require [modelo.expr :as e]))

(def ^:private +examples-enabled+ true)

(defrecord Bool []
  t/Type
  (type-name [_] "bool")
  (type-uname [_] "𝔹")
  (type-tex-name [_] "\\mathbb{B}")
  (type-describe [_] "booleans true and false")
  (type-check [_ v]
    (if (or (= v true)
            (= v false))
      :yes
      [:no v " is not a boolean true or false." v])))

(def bool (->Bool))

(example
 (t/type-check bool true) => :yes)

(example
 (t/type-check bool false) => :yes)

(example
 (t/type-check bool nil)
 => [:no nil " is not a boolean true or false." nil])

(example
 (t/type-check? bool true) => true)

(example
 (t/type-check? bool 12) => false)


(defrecord True []
  e/Expr
  (unparse [_] 'true)
  (check-type [_ t _]
    (= t bool)))

(defn mk-true
  "Build the boolean constant `true`."
  [] (->True))

(example
 (e/check-type (mk-true) bool nil)) => true

(defrecord False []
  e/Expr
  (unparse [_] 'false)
  (check-type [_ t _]
    (= t bool)))

(defn mk-false
  "Build the boolean constant `false`."
  [] (->False))

(example
 (e/check-type (mk-false) bool nil) => true)



