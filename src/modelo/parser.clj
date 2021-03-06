(ns modelo.parser
  (:require [clj-by.example :refer [example do-for-example]]))

(def ^:private +examples-enabled+ true)

(defn mk-parser
  "Create an empty parser."
  []
  {:const {}
   :extra []
   :compound {}})

(do-for-example
 (def ex-parser (atom (mk-parser))))

(declare parse-extra)

(defn parse
  "Parses an expression `e` with `parser`.

  Returns a pair of either

    - `[:yes <expr>]` if parse is successful
    - `[:no <info-map>]` in case of failure (no parser found)
    - `[:error <info-map>]` in case of error."
  ([e parser] (if (sequential? e)
                (if (seq e)
                  (if-let [sparser (get (:compound parser) (first e))]
                    (sparser e parser)
                    (parse-extra e parser))
                  (parse-extra e parser))
                ;; consts
                (if-let [aparser (get (:const parser) e)]
                  (aparser e)
                  (parse-extra e parser)))))

(defn parse-extra [e parser]
  (reduce (fn [res eparser]
            (let [res' (eparser e parser)]
              (case (first res')
                :no res
                :yes (reduced res')
                :error (reduced res'))))
          [:no {:msg "Cannot parse expression" :expr e}]
          (:extra parser)))

(defn parse-seq
  "Parses a sequence `s` of expressions with `parser`.
  Returns `[:yes r]` with `r` a sequence of parsed values in case of succes,
  or the first parse error encountered."
  [s parser]
  (loop [s s, res []]
    (if (seq s)
      (let [r (parse (first s) parser)]
        (if (= (first r) :yes)
          (recur (rest s) (conj res (second r)))
          r))
      [:yes res])))

(defn check-parse
  "Parses expression `e` with parser `parser`. 
  Returns the parsed value or raises an exception in case of failure."
  [e parser]
  (let [res (parse e parser)]
    ;; (println "[check-parse] res=" res)
    (case (first res)
      :yes (second res)
      :no (throw (ex-info "No parselet found for expression." {:expr e :info (second res)}))
      :error (throw (ex-info "Parse error" {:expr e :error (second res)})))))

(defn register-const-parselet
  "Registers in `parser` `parselet` for parsing `const`."
  [parser const parselet]
  (if (get (:const parser) const)
    (throw (ex-info "Const parser already registered."
                    {:const const}))
    (update parser :const
            (fn [ps] (assoc ps const parselet)))))

(do-for-example
 (swap! ex-parser (fn [parser] (register-const-parselet parser 'true (fn [_] [:yes true])))))

(example
 (parse 'true @ex-parser) => [:yes true])

(example
 (parse 'false @ex-parser)
 => [:no {:msg "Cannot parse expression", :expr false}])

(defn unregister-const-parselet
  "Unregisters in `parser` the parselet for `const`."
  [parser const]
  (if (get (:const parser) const)
    (update parser :const
            (fn [ps] (dissoc ps const)))
    (throw (ex-info "No such const parselet registered."
                    {:const const}))))

(example
 (:const (unregister-const-parselet @ex-parser 'true)) => {})

(defn clear-const-parselets
  "Unregister in `parser` all const parselets."
  [parser] (assoc parser :const {}))

(example
 (:const (clear-const-parselets @ex-parser)) => {})

(defn register-extra-parselet
  "Registers in `parser` `parselet` as a extra parselet."
  [parser parselet]
  (update parser :extra #(conj % parselet)))

(do-for-example
 (swap! ex-parser
        (fn [parser] (register-extra-parselet
                      parser
                      (fn [v _] (if (and (counted? v)
                                         (= (count v) 2)
                                         (integer? (first v))
                                         (keyword? (second v)))
                                  [:yes v]
                                  [:no {:msg "Not a good pair" :expr v}]))))))

(example
 (parse [12 :hello] @ex-parser) => [:yes [12 :hello]])

(example
 (parse [12 'hello] @ex-parser)
 => [:no {:msg "Cannot parse expression", :expr [12 'hello]}])

(defn clear-extra-parselets
  "Unregister in `parser` all extra parserlets."
  [parser]
  (assoc parser :extra []))

(defn register-compound-parselet
  "Registers in `parser` `parselet` as a compound parselet with first element `fst`."
  [parser fst parselet]
  (update parser :compound
          #(if (get % fst)
             (throw (ex-info "Compound parselet already registered."
                             {:first fst}))
             (assoc % fst parselet))))

(do-for-example
 (swap! ex-parser
        (fn [parser] (register-compound-parselet
                      parser 'beep
                      (fn [v p]
                        (if (and (counted? v) (= (count v) 2))
                          (let [res (parse (second v) p)]
                            (if (= (first res) :yes)
                              [:yes {:type :beep :child (second res)}]
                              [:error (second res)]))
                          [:error {:msg "Need exactly one argument for :beep."
                                   :nb-args (dec (count v))}]))))))

(example
 (parse '(beep [42 :world]) @ex-parser)
 => [:yes {:type :beep, :child [42 :world]}])

(example
 (parse '(beep 42) @ex-parser)
 => [:error {:msg "Cannot parse expression", :expr 42}])

(example
 (parse '(beep true 42) @ex-parser)
 => [:error {:msg "Need exactly one argument for :beep.", :nb-args 2}])

(defn unregister-compound-parselet
  "Unregisters in `parser` the compound parselet for `fst`."
  [parser fst]
  (if (get (:compound parser) fst)
    (update parser :compound
            (fn [ps] (dissoc ps fst)))
    (throw (ex-info "No such compound parselet registered."
                    {:first fst}))))

(example
 (:compound (unregister-compound-parselet @ex-parser 'beep)) => {})

(defn clear-compound-parselets
  "Unregister in `parser` all compound parselets."
  [parser] (assoc parser :compound {}))

(defn clear-parselets
  "Unregister all parselets in `parser`."
  [parser]
  (-> parser
      (clear-const-parselets)
      (clear-compound-parselets)
      (clear-extra-parselets)))

(do-for-example
 (swap! ex-parser clear-parselets))

(example
 (= @ex-parser (mk-parser)) => true)

