
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
the type environment `env`.")


  )


(defrecord Var [name kind]
  Expr
  (unparse [e] (:name e))
  (check-type [e ty env]
    (= (tenv/fetch env e)
       ty)))

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







