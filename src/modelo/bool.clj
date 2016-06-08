
(ns modelo.bool
  (:require [clj-by.example :refer [example]])
  (:require [modelo.type :as t])
  (:require [modelo.expr :as e]))

(def ^:private +examples-enabled+ true)

;;{
;; # Bool type
;;}

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

;;{
;; # Constant true
;;}

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

(defn parse-true [_]
  [:yes (mk-true)])

(defn register-true-parselet! []
  (e/register-const-parselet! true parse-true))

;;{
;; # Constant false
;;}

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

(defn parse-false [_]
  [:yes (mk-false)])

(defn register-false-parselet! []
  (e/register-const-parselet! false parse-false))

;;{
;; # Negation
;;}

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

(defn parse-not [e _]
  (if (not= (count e) 2)
    [:error {:msg "Wrong arity for `not`." :arity-expect 2 :arity-given (dec (count e))
             :expr e}]
    (let [res (e/parse-expr (second e))]
      (if (= (first res) :yes)
        [:yes (mk-not (second res))]
        [:error (second res)]))))

(defn register-not-parselet! []
  (e/register-compound-parselet! 'not parse-not))

;;{
;; # Conjunction
;;}

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

(defn parse-and [e _]
  (if (< (count e) 3) 
    [:error {:msg "Operator `and` needs at least 2 arguments." :nb-args (dec (count e))
             :expr e}]
    (let [res (e/parse-seq (rest e))]
      ;;(println "[parse-and] res = " res)
      (if (= (first res) :yes)
        [:yes (apply mk-and (second res))]
        [:error (second res)]))))

(defn register-and-parselet! []
  (e/register-compound-parselet! 'and parse-and))

;;{
;; # Disjunction
;;}

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

(defn parse-or [e _]
  (if (< (count e) 3) 
    [:error {:msg "Operator `or` needs at least 2 arguments." :nb-args (dec (count e))
             :expr e}]
    (let [res (e/parse-seq (rest e))]
      (if (= (first res) :yes)
        [:yes (apply mk-or (second res))]
        [:error (second res)]))))

(defn register-or-parselet! []
  (e/register-compound-parselet! 'or parse-or))

;;{
;; # Implication
;;}

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

(defn parse-imply [e _]
  (if (not= (count e) 3) 
    [:error {:msg "Operator `imply` needs exactly 2 arguments." :nb-args (dec (count e))
             :expr e}]
    (let [hres (e/parse-expr (nth e 1))]
      (if (not= (first hres) :yes)
        [:error (second hres)]
        (let [cres (e/parse-expr (nth e 2))]
          (if (not= (first cres) :yes)
            [:error (second cres)]
            [:yes (mk-imply (second hres) (second cres))]))))))

(defn register-imply-parselet! []
  (e/register-compound-parselet! 'imply parse-imply)
  (e/register-compound-parselet! '==> parse-imply))

;;{
;; # Equivalence
;;}

(defrecord Iff [hyp concl]
  e/Expr
  (unparse [expr] (list 'iff (e/unparse hyp) (e/unparse concl)))
  (check-type [expr ty env]
    (if (not= ty bool)
      {:status :type-error :msg "equivalence returns a boolean"
       :expected bool
       :given ty}
      (let [res1 (e/check-type (:hyp expr) bool env)]
        (if (not= (:status res1) :ok)
          res1
          (e/check-type (:concl expr) bool env))))))

(defn mk-iff
  "Build a logical equivalence with operands `left` and `right`."
  [left right] (->Iff left right))

(example
 (e/unparse (mk-iff (mk-true) (mk-false))) => '(iff true false))

(example
 (e/check-type (mk-iff (mk-true) (mk-false)) bool nil) => {:status :ok})

(defn parse-iff [e _]
  (if (not= (count e) 3) 
    [:error {:msg "Operator `iff` needs exactly 2 arguments." :nb-args (dec (count e))
             :expr e}]
    (let [lres (e/parse-expr (nth e 1))]
      (if (not= (first lres) :yes)
        [:error (second lres)]
        (let [rres (e/parse-expr (nth e 2))]
          (if (not= (first rres) :yes)
            [:error (second rres)]
            [:yes (mk-iff (second lres) (second rres))]))))))

(defn register-iff-parselet! []
  (e/register-compound-parselet! 'iff parse-iff)
  (e/register-compound-parselet! '<=> parse-iff))

;;{
;; # Parselets registration
;;}

(defn register-parselets!
  "Install the boolean expressions parselets."
  []
  (register-true-parselet!)
  (register-false-parselet!)
  (register-not-parselet!)
  (register-and-parselet!)
  (register-or-parselet!)
  (register-imply-parselet!)
  (register-iff-parselet!)
  )

