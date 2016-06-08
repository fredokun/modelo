
(ns modelo.type
  (:require [clj-by.example :refer [example do-for-example]])
  (:require [modelo.parser :as p])
  )

(def ^:private +examples-enabled+ true)

(defprotocol Type
  "Basic API for Modelo types."

  (type-name [t]
    "Get the descriptive name of the type.")

  (type-uname [t]
    "The descriptive name of the type in Mathematics-enabled unicode.")

  (type-tex-name [t]
    "The tex/latex name of the type.")

  (type-describe [t]
    "The description of the type.")

  (type-check [t v]
    "Check if value `v` is of type `t`.")

  (type-unparse [t]
    "Produces a repl-friendly representation of the type `t`."))

(defn type-check?
  "Returns `true` if value `v` is of type `t`,
  and `false` otherwise."
  [t v]
  (= (type-check t v) :yes))


(def ^:private +type-parser+ (atom (p/mk-parser)))

(defn parse-type
  "Parses type expression `e`. Returns a pair of either

    - `[:yes <result>]` if parse is successful
    - `[:no <info-map>]` in case of failure (no parser found)
    - `[:error <info-map>]` in case of error."
  [e] (p/parse e @+type-parser+))

(defn check-parse-type
  "Parses type expression `e`. Returns the parsed value or
raises an exception in case of failure."
  [e] (p/check-parse e @+type-parser+))

(defn parse-seq
  "Parses the sequence `s` of type expressions.
  Returns `[:yes r]` with `r` a sequence of parsed values in case of succes,
  or the first parse error encountered."
  [s] (p/parse-seq s @+type-parser+))

(defn register-const-parselet!
  "Registers `parselet` as a const parselet for `const`."
  [const parselet]
  (swap! +type-parser+ #(p/register-const-parselet % const parselet)))

(defn unregister-const-parselet!
  "Unregisters the parselet for `const`."
  [const]
  (swap! +type-parser+ #(p/unregister-const-parselet % const)))

(defn clear-const-parserlets!
  "Unregister all const parselets."
  []
  (swap! +type-parser+ p/clear-const-parselets))

(defn register-extra-parselet!
  "Registers `parselet` as a extra parselet."
  [parselet]
  (swap! +type-parser+ #(p/register-extra-parselet % parselet)))

(defn clear-extra-parselets!
  "Unregister all extra parselets."
  []
  (swap! +type-parser+ p/clear-extra-parselets))

(defn register-compound-parselet!
  "Registers `parselet` as a compound parselet with first element `fst`."
  [fst parser]
  (swap! +type-parser+ #(p/register-compound-parselet % fst parser)))

(defn unregister-compound-parselet!
  "Unregisters the compound parselet for `fst`."
  [fst]
  (swap! +type-parser+ #(p/unregister-compound-parselet % fst)))

(defn clear-compound-parselets!
  "Unregister all compound parselets."
  []
  (swap! +type-parser+ p/clear-compound-parselets))

(defn clear-parselets!
  "Unregister all parselets."
  []
  (swap! +type-parser+ p/clear-parselets))




;; (defrecord Int []
;;   Type
;;   (type-name [_] "int")
;;   (type-uname [_] "ℤ")
;;   (type-tex-name [_] "\\mathbb{Z}")
;;   (type-describe [_] "integers -5, -2, 0, 2, 5,...")
;;   (type-check [_ v]
;;     (if (integer? v)
;;       :yes
;;       [:no v "is not an integer"])))

;; (def int (->Int))

;; (example
;;  (type-check int 42) => :yes)

;; (example
;;  (type-check int -42) => :yes)

;; (example
;;  (type-check int true) => [:no true "is not an integer"])

;; (example
;;  (type-check? int 42) => true)

;; (declare set-check-elems)

;; (defrecord Set [elem-type]
;;   Type
;;   (type-name [t] (str "Set[" (type-name (:elem-type t)) "]"))
;;   (type-uname [t] (str "Set[" (type-uname (:elem-type t)) "]"))
;;   (type-tex-name [t] (str "Set[" (type-tex-name (:elem-type t)) "]"))
;;   (type-describe [t] (str "sets of " (type-describe (:elem-type t))))

;;   (type-check [t v]
;;     (if (not (set? v))
;;       [:no v "is not a set"]
;;       (set-check-elems (:elem-type t) v))))

;; (defn set-check-elems
;;   "Check that all the elements of set `s` are of type `t`."
;;   [t s]
;;   (if (seq s)
;;     (let [res (type-check t (first s))]
;;       (if (= res :yes)
;;         (recur t (rest s))
;;         res))
;;     :yes))

;; (do-for-example
;;  (def int-set (->Set int)))

;; (example
;;  (type-check int-set #{1 3 2 9}) => :yes)

;; (example
;;  (type-check int-set '(1 2 3)) => [:no '(1 2 3) "is not a set"])

;; (example
;;  (type-check int-set #{1 3 :two 9}) => [:no :two "is not an integer"])

;; (declare product-type-check)

;; (defrecord Product [types]
;;   Type
;;   (type-name [t] (clojure.string/join " * " (map type-name (:types t))))
;;   (type-uname [t] (clojure.string/join " × " (map type-uname (:types t))))
;;   (type-tex-name [t] (clojure.string/join " \\times " (map type-tex-name (:types t))))
;;   (type-describe [t] (str "tuples of " (clojure.string/join ", and " (map type-describe (:types t)))))

;;   (type-check [t v]
;;     (cond
;;       (not (counted? v))
;;       [:no v "is not counted."]
;;       (not= (count v) (count (:types t)))
;;       [:no v (str "has incorrect arity (expecting " (count (:types t)) " given " (count v) ")")]
;;       :else (product-type-check (:types t) v))))

;; (defn product-type-check
;;   "Check that all the counted sequence of values `vs` have types `ts`."
;;   [ts vs]
;;   (if (seq ts)
;;     (let [tc (type-check (first ts) (first vs))]
;;       (if (= tc :yes)
;;         (recur (rest ts) (rest vs))
;;         tc))
;;     :yes))

;; (defn pair
;;   "Build the type of a pairs in `t1 * t2`."
;;   [t1 t2]
;;   (->Product [t1  t2]))

;; (example
;;  (type-name (pair bool int)) => "bool * int")

;; (example
;;  (type-describe (pair bool int))
;;  => "tuples of booleans true and false, and integers -5, -2, 0, 2, 5,...")

;; (example
;;  (type-check (pair bool int) [true 42]) => :yes)

;; (example
;;  (type-check (pair bool int) [2 false])
;;  => [:no 2 " is not a boolean true or false." 2])

;; (example
;;  (type-check (pair bool int) [1 2 3])
;;  => [:no [1 2 3] "has incorrect arity (expecting 2 given 3)"])

;; (example
;;  (type-check (pair bool int) 42) => [:no 42 "is not counted."])





