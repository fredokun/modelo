
(ns modelo.type-env
  (:require [clj-by.example :refer [example do-for-example]])
  )

(def ^:private +examples-enabled+ true)

;;{

;; # Type environments

;; A type environment is a binding from term variables
;; to types, with possible shadowing. Hence, the
;; type environment is represented as a vector
;; of pairs `[v t]` with `v` a variable and `t` a type.

(defn fetch [env v]
  "Fetch the type of variable `v` in environment `env`.
Returns `nil` if there is no such variable."
  (reduce (fn [res [v' t]]
            (if (= v v')
              (reduced t)
              res)) nil env))

(example
 (fetch '[[x int] [y nat] [z bool]] 'y)
 => 'nat)

(example
 (fetch '[[x int] [x nat] [z bool]] 'x)
 => 'int)

(example
 (fetch '[[x int] [x nat] [z bool]] 'y)
 => nil)



