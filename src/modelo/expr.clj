
(ns modelo.expr
  (:require [clj-by.example :refer [example do-for-example]])
  (:require [modelo.type-env :as tenv])
  (:require [modelo.parser :as p]))

(def ^:private +examples-enabled+ true)

(defprotocol Expr
  "Basic API for Modelo expressions."

  (unparse [e]
    "Produce a repl-friendly representation of the expression `e`.")

  (check-type [e ty env]
    "Check that the type of the expression `e` is of type `ty` in 
the type environment `env`.
Returns a map with `:status` `:ok` is type checking is ok,
or `:type-error` in case of a type error. Other errors and related
informations about the errors can be returned in the map.")

  )

(def ^:private +expr-parser+ (atom (p/mk-parser)))

(defn parse-expr
  "Parses expression `e`. Returns a pair of either

    - `[:yes <expr>]` if parse is successful
    - `[:no <info-map>]` in case of failure (no parser found)
    - `[:error <info-map>]` in case of error."
  [e] (p/parse e @+expr-parser+))

(defn check-parse-expr
  "Parses expression `e`. Returns the parsed value or
raises an exception in case of failure."
  [e] (p/check-parse e @+expr-parser+))

(defn parse-seq
  "Parses the sequence `s` of expressions.
  Returns `[:yes r]` with `r` a sequence of parsed values in case of succes,
  or the first parse error encountered."
  [s] (p/parse-seq s @+expr-parser+))

(defn register-const-parselet!
  "Registers `parselet` as a const parselet for `const`."
  [const parselet]
  (swap! +expr-parser+ #(p/register-const-parselet % const parselet)))

(defn unregister-const-parselet!
  "Unregisters the parselet for `const`."
  [const]
  (swap! +expr-parser+ #(p/unregister-const-parselet % const)))

(defn clear-const-parserlets!
  "Unregister all const parselets."
  []
  (swap! +expr-parser+ p/clear-const-parselets))

(defn register-extra-parselet!
  "Registers `parselet` as a extra parselet."
  [parselet]
  (swap! +expr-parser+ #(p/register-extra-parselet % parselet)))

(defn clear-extra-parselets!
  "Unregister all extra parselets."
  []
  (swap! +expr-parser+ p/clear-extra-parselets))

(defn register-compound-parselet!
  "Registers `parselet` as a compound parselet with first element `fst`."
  [fst parser]
  (swap! +expr-parser+ #(p/register-compound-parselet % fst parser)))

(defn unregister-compound-parselet!
  "Unregisters the compound parselet for `fst`."
  [fst]
  (swap! +expr-parser+ #(p/unregister-compound-parselet % fst)))

(defn clear-compound-parselets!
  "Unregister all compound parselets."
  []
  (swap! +expr-parser+ p/clear-compound-parselets))

(defn clear-parselets!
  "Unregister all parselets."
  []
  (swap! +expr-parser+ p/clear-parselets))

(defn check-type-exprs
  "Check that all the elements of the sequence `exprs` have the same
  type `ty` in type environment `env`."
  [exprs ty env]
  (reduce (fn [_ expr]
            (let [res (check-type expr ty env)]
              (if (= (:status res) :ok)
                res
                (reduced res)))) {:status :ok} exprs))

(defrecord Var [name kind]
  Expr
  (unparse [e] (:name e))
  (check-type [e ty env]
    (if-let [vty (tenv/fetch env e)]
      (if (= vty ty)
        {:status :ok}
        {:status :type-error :msg "mismatch variable type"
         :expected ty
         :given vty})
      {:status :var-error :msg "no such variable" :variable e})))

(defn mk-var
  "Create a variable with the specified `name` and `kind`."
  [name kind]
  (->Var name kind))

(example
 (:name (mk-var 'x :free)) => 'x)

(example
 (:kind (mk-var 'x :free)) => :free)

(example
 (unparse (mk-var 'x :free)) => 'x)

(defn parse-var [e _]
  (if (symbol? e)
    [:yes (mk-var e :unspecified)]
    [:no {:msg "not a variable" :expr e}]))

(defn register-var-parselet!
  "Register the parselet for variables."
  [] (register-extra-parselet! parse-var))



