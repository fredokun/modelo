
(ns modelo.expr
  (:require [clj-by.example :refer [example do-for-example]])
  (:require [modelo.type-env :as tenv]))

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







