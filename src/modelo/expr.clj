
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

(def ^:private +expr-parser+ (p/mk-parser))

(defn parse-expr
  "Parses an expression `e`. Returns a pair of either

    - `[:yes <expr>]` if parse is successful
    - `[:no <info-map>]` in case of failure (no parser found)
    - `[:error <info-map>]` in case of error."
  [e] (p/parse e @+expr-parser+))

(defn register-const-parser!
  "Registers `parser` as a const parser for `const`."
  [const parser] (p/register-const-parser! +expr-parser+ const parser))

(defn unregister-const-parser!
  "Unregisters the parser for `const`."
  [const] (p/unregister-const-parser! +expr-parser+ const))

(defn clear-const-parsers!
  "Unregister all const parsers."
  [] (p/clear-const-parsers! +expr-parser+))

(defn register-extra-parser!
  "Registers `parser` as a extra parser."
  [parser] (p/register-extra-parser! +expr-parser+ parser))

(defn clear-extra-parsers!
  "Unregister all extra parsers."
  [] (p/clear-extra-parsers! +expr-parser+))

(defn register-compound-parser!
  "Registers `parser` as a compound parser with first element `fst`."
  [fst parser] (p/register-compound-parser! +expr-parser+ fst parser))

(defn unregister-compound-parser!
  "Unregisters the compound parser for `fst`."
  [fst] (p/unregister-compound-parser! +expr-parser+ fst))

(defn clear-compound-parsers!
  "Unregister all compound parsers."
  [] (p/clear-compound-parsers! +expr-parser+))

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

(defn parse-var [e]
  (if (symbol? e)
    [:yes (mk-var e :unspecified)]
    [:no {:msg "not a variable" :expr e}]))

(defn register-var-parser!
  "Register the parser for variables."
  [] (register-extra-parser! parse-var))


