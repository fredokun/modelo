
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
    (if (= t bool)
      {:status :ok}
      {:status :type-error :msg "bool expected" :expected bool :given t})))

(defn mk-true
  "Build the boolean constant `true`."
  [] (->True))

(example
 (e/check-type (mk-true) bool nil) => {:status :ok})

(defrecord False []
  e/Expr
  (unparse [_] 'false)
  (check-type [_ t _]
    (if (= t bool)
      {:status :ok}
      {:status :type-error :msg "bool expected" :expected bool :given t})))

(defn mk-false
  "Build the boolean constant `false`."
  [] (->False))

(example
 (e/check-type (mk-false) bool nil) => {:status :ok})

(example
 (e/check-type (mk-false) :int nil) =>
 {:status :type-error, :msg "bool expected",
  :expected #modelo.bool.Bool{}, :given :int})

(defrecord Not [arg]
  e/Expr
  (unparse [n] (list 'not (e/unparse (:arg n))))
  (check-type [n t env]
    (if (not= t bool)
      {:status :type-error :msg "not operator returns a boolean"
       :expected bool
       :given t}
      (e/check-type (:arg n) t env))))

(defn mk-not
  "Build the negation of `arg`."
  [arg]
  (->Not arg))

(example
 (e/unparse (mk-not (mk-not (mk-true)))) => '(not (not true)))

(example
 (e/check-type (mk-not (mk-true)) bool nil) => {:status :ok})

(example
 (e/check-type (mk-not (mk-true)) :int nil)
 => {:status :type-error,
     :msg "not operator returns a boolean",
     :expected #modelo.bool.Bool{},
     :given :int})

(example
 (e/check-type (mk-not (e/mk-var 'x :free)) bool nil)
 =>
 {:status :var-error,
  :msg "no such variable",
  :variable #modelo.expr.Var{:name x, :kind :free}})

(defrecord And [args]
  e/Expr
  (unparse [expr] (conj (into '() (map e/unparse (:args expr))) 'and))
  (check-type [expr ty env]
    (if (not= ty bool)
      {:status :type-error :msg "and operator returns a boolean"
       :expected bool
       :given ty}
      (e/check-type-exprs (:args expr) ty env))))

(defn mk-and
  "Build a logical and expression with arguments `arg1`, `arg2` and `more`."
  [arg1 arg2 & more]  (->And (cons arg1 (cons arg2 more))))

(example
 (e/unparse (mk-and (mk-not (mk-true)) (mk-false) (mk-not (mk-false))))
 => '(and (not false) false (not true)))

(example
 (e/check-type (mk-and (mk-not (mk-true)) (mk-false) (mk-not (mk-false)))
               bool nil) => {:status :ok})

(example
 (e/check-type (mk-and (mk-not (mk-true)) (mk-false) (mk-not (mk-false)))
               :int nil)
 =>
 {:status :type-error, :msg "and operator returns a boolean",
  :expected #modelo.bool.Bool{}, :given :int})

(defrecord Or [args]
  e/Expr
  (unparse [expr] (conj (into '() (map e/unparse (:args expr))) 'or))
  (check-type [expr ty env]
    (if (not= ty bool)
      {:status :type-error :msg "or operator returns a boolean"
       :expected bool
       :given ty}
      (e/check-type-exprs (:args expr) ty env))))

(defn mk-or
  "Build a logical or expression with arguments `arg1`, `arg2` or `more`."
  [arg1 arg2 & more]  (->Or (cons arg1 (cons arg2 more))))

(example
 (e/unparse (mk-or (mk-not (mk-true)) (mk-false) (mk-not (mk-false))))
 => '(or (not false) false (not true)))

(example
 (e/check-type (mk-or (mk-not (mk-true)) (mk-false) (mk-not (mk-false)))
               bool nil) => {:status :ok})

(example
 (e/check-type (mk-or (mk-not (mk-true)) (mk-false) (mk-not (mk-false)))
               :int nil)
 =>
 {:status :type-error, :msg "or operator returns a boolean",
  :expected #modelo.bool.Bool{}, :given :int})

(defrecord Imply [hyp concl]
  e/Expr
  (unparse [expr] (list 'imply (e/unparse hyp) (e/unparse concl)))
  (check-type [expr ty env]
    (if (not= ty bool)
      {:status :type-error :msg "implication returns a boolean"
       :expected bool
       :given ty}
      (let [res1 (e/check-type (:hyp expr) bool env)]
        (if (not= (:status res1) :ok)
          res1
          (e/check-type (:concl expr) bool env))))))

(defn mk-imply
  "Build a logical implication with hypothesis `hyp` and conclusion `concl`."
  [hyp concl] (->Imply hyp concl))

(example
 (e/unparse (mk-imply (mk-true) (mk-false))) => '(imply true false))

(example
 (e/check-type (mk-imply (mk-true) (mk-false)) bool nil) => {:status :ok})

