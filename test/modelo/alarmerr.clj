
(ns modelo.alarmerr
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clj-by.example :refer [example do-for-example]]))

(def ^:private +examples-enabled+ true)

;;{

;; This is a Modelo port of a classical VDM example

;;}

;; (def-type Qualifification
;;     "The type of qualifications."
;;   (enum :elec :mech :bio :chem))

(s/def ::qualification #{:elem :mech :bio :chem})

;; (s/exercise ::qualification)

;; (def-type ExpertId
;;     "The identifier of an expert.")

(s/def ::expert-id string?)

;; (def-type Expert
;;     "The type of experts."
;;   {:id ExpertId
;;    :quali (set Qualification)})

(s/def ::expert-qualis (s/coll-of ::qualification :kind set? :min-count 1))

(s/def ::expert (s/keys :req-un [::expert-id ::expert-qualis]))


(do-for-example
 (def expert-george {:expert-id "george"
                     :expert-qualis #{:mech :bio}}))

(example
 (s/valid? ::expert expert-george)
 => true)

(example
 (s/valid? ::expert {:expert-id "george"
                     :expert-qualis #{}})
 => false)

;; (s/exercise ::expert)

;; (def-type-invariant Expert inv-1 [ex]
;;   (not= (:quali ex) #{}))

;; this invariant can be directly expressed in spec

;; (def-type Alarm
;;     "The type of alarms."
;;   {:alarmtext : string
;;    :quali Qualification})

(s/def ::alarm-text string?)
(s/def ::alarm-quali ::qualification)

(s/def ::alarm (s/keys :req-un [::alarm-text ::alarm-quali]))

(do-for-example
 (def alarm-1 {:alarm-text "bip bip"
               :alarm-quali :mech}))

(example
 (s/valid? ::alarm alarm-1)
 => true)


;; (def-type Period
;;     "The abstract types of periods.")

(s/def ::period keyword?)

;; (def-type Schedule
;;     "The type of schedules."
;;     (map Period (set Expert)))

(s/def ::experts (s/coll-of ::expert :kind set? :min-count 1))

(s/def ::schedule (s/map-of ::period ::experts))

;; (s/exercise ::schedule)

(do-for-example
 (def expert-marcel {:expert-id "marcel"
                     :expert-qualis #{:bio}})

 (def expert-emily {:expert-id "emily"
                    :expert-qualis #{:mech :elem}})

 (def expert-john {:expert-id "john"
                   :expert-qualis #{:elem}}))

(do-for-example
 (def schedule-1 {:shift1 #{expert-marcel expert-emily expert-george}
                  :shift2 #{expert-john}}))

(example
 (s/valid? ::schedule schedule-1)
 => true)

(example
 (s/valid? ::schedule {:shift1 #{{:expert-id "marcel"
                                  :expert-qualis #{:bio}}
                                 {:expert-id "emily"
                                  :expert-qualis #{:mech :elem}}}
                       :shift2 #{{:expert-id "john"
                                  :expert-qualis #{:elem}}}
                       :shift3 #{}})
 => false)

(example
 (s/valid? ::schedule {:shift1 #{{:expert-id "marcel"
                                  :expert-qualis #{}}
                                 {:expert-id "emily"
                                  :expert-qualis #{:mech :elem}}}
                       :shift2 #{{:expert-id "john"
                                 :expert-qualis #{:elem}}}})
 => false)

;; (def-type-invariant Schedule inv-1 [sch]
;;   (forall [exs (range sch)]
;;      (not= exs #{})))

;; already in spec

;; (def-type Plant
;;     "The main Plant type"
;;   {:schedule Schedule
;;    :alarms (set Alarm)})

(s/def ::plant-schedule ::schedule)
(s/def ::plant-alarms (s/coll-of ::alarm :kind set?))

(s/def ::plant (s/keys :req-un [::plant-schedule ::plant-alarms]))

;; (s/exercise ::plant)

(do-for-example
 (def plant-1 {:plant-schedule schedule-1
               :plant-alarms #{alarm-1}}))

(example
 (s/valid? ::plant plant-1) => true)

;; (def-type-invariant Plant inv-1 [p]
;;   (forall [a (:alarms p)
;;            per (dom (:schedule p))]
;;      (qualification-ok ((:schedule p) per) (:quali a) a)))




;; (def-function number-of-experts
;;   "The number of experts during Period `per` in Plant `pt`."
;;   {:pre (contains? (dom (:schedule pt)) per)}
;;   [[per Period] [pt Plant] -> nat]
;;   (count ((:schedule plant) per)))

(defn number-of-experts [plant period]
  (count ((:plant-schedule plant) period)))

(example
 (number-of-experts plant-1 :shift1) => 3)

(example
 (number-of-experts plant-1 :shift2) => 1)

(s/fdef number-of-experts
        :args (s/and (s/cat :plant ::plant :period ::period)
                     #(contains? (:plant-schedule (:plant %)) (:period %)))
        :ret? int?)

(stest/instrument `number-of-experts)

(example
 (number-of-experts plant-1 :shift1) => 3)

(example
 (number-of-experts plant-1 :shift2) => 1)

(contains? (:plant-schedule plant-1) :shift1)

(example
 (try
   (number-of-experts plant-1 :shift3)
   (catch Exception e
     :s/invalid))
 => :s/invalid)


;; (def-function expert-duty-periods
;;   "The periods when Expert `ex` is on duty in Plant `pt`."
;;   [[ex Expert] [pt Plant] -> (set Period)]
;;   (mk-set [[per Period] (dom (:schedule pt))]
;;       per))

;; (def-function expert-to-page
;;   "The expert to page for Alarm `a` signaled in Period `per` in Plant `pt`."
;;   {:pre (and (contains? (dom (:schedule pt)) per)
;;              (contains? (:alarms pt) q))}
;;   {:post (contains? (:quali a) (:quali %))}
;;   [[a Alarm] [per Period] [pt Plant] -> Expert])
