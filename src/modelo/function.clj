(ns modelo.function
  (:require [clj-by.example :refer [example do-for-example]])
  (:require [modelo.types :as t])
  (:require [modelo.expr :as e]))

(def ^:private +examples-enabled+ true)

(defrecord Function [name doc params ret pre post body])

(defn mk-function
  "Build a function with the specified `name`, `doc`umentation string,
`param`eter`s`, `ret`urn type, `pre`condition, `post`condition and `body`.
 An implicit function has no body and a post-condition. An explicit
  function has a body and no postcondition."
  [name doc params ret pre post body]
  (->Function name doc params ret pre post body))


(defn implicit? [f]
  (nil? (:body f)))

(defn explicit? [f]
  (not (implicit? f)))

(do-for-example
 (def ex-fun-implicit
   (mk-function 'same-implicit
                "A dummy implicit boolean function."
                [['b modelo.bool/bool]] modelo.bool/bool
                (modelo.bool/mk-true)
                (modelo.bool/mk-iff (e/mk-var 'b :param)
                                    (e/mk-var '% :ret))
                nil)))

(example
 (implicit? ex-fun-implicit) => true)

(do-for-example
 (def ex-fun-explicit
   (mk-function 'same-explicit
                "A dummy explicit boolean function."
                [['b modelo.bool/bool]] modelo.bool/bool
                (modelo.bool/mk-true)
                (modelo.bool/mk-iff (e/mk-var 'b :param)
                                    (e/mk-var '% :ret))
                (modelo.bool/mk-not
                 (modelo.bool/mk-not (e/mk-var 'b :param))))))

(example
 (explicit? ex-fun-explicit) => true)


