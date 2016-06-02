
(ns modelo.expr
  (:require [clj-by.example :refer [example do-for-example]])
  (:require [modelo.type-env] :as tenv))

(def ^:private +examples-enabled+ true)

(defprotocol Expr
  "Basic API for Modelo expressions."

  (expr-unparse [e]
                "Produce a repl-friendly representation of the expression `e`.")
  
  (expr-type [e env]
             "Get the type of the expression `e` in the type environment `env`.")


  )


(defrecord
 Var [name kind]
 Expr
 (expr-unparse [e] (:name e))
 (expr-type [e env] (tenv/fetch env e)))


