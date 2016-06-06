(ns modelo.parser)

(defn mk-parser
  "Create an empty parser."
  []
  (atom {:const {}
         :extra {}
         :compound {}}))

(defn parse
  "Parses an expression `e` with `parser`.

  Returns a pair of either

    - `[:yes <expr>]` if parse is successful
    - `[:no <info-map>]` in case of failure (no parser found)
    - `[:error <info-map>]` in case of error."
  ([e parser] (if (seq e)
                (if-let [sparser (get (:compound parser) (first e))]
                  (sparser e parser)
                  (reduce (fn [res lparser]
                            (let [res' (lparser e parser)]
                              (case (first res')
                                :no res
                                :yes (reduce res')
                                :error (reduce res'))))
                          [:no {:msg "Cannot parse expression" :expr e}]
                          (:extra parser)))
                ;; consts
                (if-let [aparser (get (:const parser) e)]
                  (aparser e)
                  [:error {:msg "No parser for const" :expr e}]))))

(defn register-const-parser!
  "Registers in `atom` `parser` as a const parser for `const`."
  [atom const parser]
  (swap! atom (fn [p] (if (get (:const p) const)
                        (throw (ex-info "Const parser already registered."
                                        {:const const}))
                        (update p :const
                                (fn [ps] (assoc ps const parser)))))))

(defn unregister-const-parser!
  "Unregisters in `atom` the parser for `const`."
  [atom const]
  (swap! atom (fn [p] (if (get (:const p) const)
                        (update p :const
                                (fn [ps] (dissoc ps const)))
                        (throw (ex-info "No such const parser registered."
                                        {:const const}))))))

(defn clear-const-parsers!
  "Unregister in `atom` all const parsers."
  [atom]
  (swap! atom #(assoc % :const {})))

(defn register-extra-parser!
  "Registers in `atom` `parser` as a extra parser."
  [atom parser]
  (swap! atom (fn [p] (update p :extra #(conj % parser)))))

(defn clear-extra-parsers!
  "Unregister in `atom` all extra parsers."
  [atom]
  (swap! atom #(assoc % :extra {})))

(defn register-compound-parser!
  "Registers in `atom` `parser` as a compound parser with first element `fst`."
  [atom fst parser]
  (swap! atom
         (fn [p] (update p :compound #(if (get % fst)
                                        (throw (ex-info "Compound parser already registered."
                                                        {:first fst}))
                                        (assoc % fst parser))))))

(defn unregister-compound-parser!
  "Unregisters in `atom` the compound parser for `fst`."
  [atom fst]
  (swap! atom (fn [p] (if (get (:compound p) fst)
                        (update p :compound
                                (fn [ps] (dissoc ps fst)))
                        (throw (ex-info "No such compound parser registered."
                                        {:first fst}))))))


(defn clear-compound-parsers!
  "Unregister in `atom` all compound parsers."
  [atom]
  (swap! atom #(assoc % :compound {})))



