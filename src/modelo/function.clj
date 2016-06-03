(ns modelo.function
  (:require [clj-by.example :refer [example]])
  (:require [modelo.types :as type])
  (:require [modelo.expr :as expr]))

(defrecord Function [name doc sig pre post body])

(defn mk-function
  "Build a function with the specified `name`, `doc`umentation string,
`sig`nature, `pre`condition, `post`condition and `body`.
 An implicit function has no body and a post-condition. An explicit
  function has a body and no postcondition."
  [name doc sig pre post body]
  (->Function name doc sig pre post body))


(defn implicit? [f]
  (nil? (:body f)))

(defn explicit? [f]
  (not (implicit? f)))

