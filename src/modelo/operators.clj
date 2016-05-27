
(ns modelo.operators)

(defprotocol Operator
  "Basic API for Modelo operators."

  (op-name [op]
    "the descriptive name of the operator.")

  (op-uname [op]
    "the descriptive name of the operator in Mathematics-enabled unicode.")

  (op-sig [op]
    "the signature of the operator.")

  (op-meta [op]
    "meta-data about the operator (fixity, priority, etc.)"))

