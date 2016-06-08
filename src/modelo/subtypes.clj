
(ns modelo.subtypes
  (:require [clj-by.example :refer [example do-for-example]]
            [modelo.type :as t :refer [type-check]])
  (:import [modelo.type.Type]))

(def ^:private +examples-enabled+ true)

(declare check-invariant)

(defrecord Subtype [name uname tex-name describe type invariant]
  modelo.type.Type
  (type-name [t] (:name t))
  (type-uname [t] (:uname t))
  (type-tex-name [t] (:tex-name t))
  (type-describe [t] (:describe t))
  (type-check [t v]
    (let [tc (type-check (:type t) v)]
      (if (= tc :yes)
        (check-invariant (:invariant t) v)
        tc))))

(defn check-invariant [[inv msg] v]
  (if (inv v)
    :yes
    [:no v msg]))

;; (def nat (->Subtype "nat" "ℕ" "\\mathbb{N}"
;;                     "natural integers 0, 1, 2, ..."
;;                     types/int
;;                     [#(>= % 0), "should be a positive integer"]))

;; (example
;;  (type-check nat 12) => :yes)

;; (example
;;  (type-check nat -12) => [:no -12 "should be a positive integer"])

;; (example
;;  (type-check nat true) => [:no true "is not an integer"])



