
(ns modelo.bool
  (:require [clj-by.example :refer [example]])
  (:require [modelo.types :as t]))

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



(example
 (t/type-parse 'bool) => bool)
