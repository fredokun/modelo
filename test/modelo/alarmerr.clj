
(ns modelo.alarmerr
  (:require [modelo.spec :as spec :refer [def-type deftype-invariant def-function]]))

;;{

;; This is a Modelo port of a classical VDM example

;;}

(def-type Plant
    "The main Plant type"
  {:schedule Schedule
   :alarms (set Schedule)})

(def-type-invariant Plant inv-1 [p]
  (forall [a (:alarms p)
           per (dom (:schedule p))]
     (qualification-ok ((:schedule p) per) (:quali a) a)))

(def-type Schedule
    "The type of schedules."
    (map Period (set Expert)))

(def-type-invariant Schedule inv-1 [sch]
  (forall [exs (range sch)]
     (not= exs #{})))

(def-type Period
    "The abstract types of periods.")

(def-type Expert
    "The type of experts."
  {:id ExpertId
   :quali (set Qualification)})

(def-type-invariant Expert inv-1 [ex]
  (not= (:quali ex) #{}))

(def-type ExpertId
    "The identifier of an expert.")

(def-type Qualifification
    "The type of qualifications."
  (enum :elec :mech :bio :chem))

(def-type Alarm
    "The type of alarms."
  {:alarmtext : string
   :quali Qualification})

(def-function number-of-experts
  "The number of experts during Period `per` in Plant `pt`."
  {:pre (contains? (dom (:schedule pt)) per)}
  [[per Period] [pt Plant] -> nat]
  (count ((:schedule plant) per)))

(def-function expert-duty-periods
  "The periods when Expert `ex` is on duty in Plant `pt`."
  [[ex Expert] [pt Plant] -> (set Period)]
  (mk-set [[per Period] (dom (:schedule pt))]
      per))

(def-function expert-to-page
  "The expert to page for Alarm `a` signaled in Period `per` in Plant `pt`."
  {:pre (and (contains? (dom (:schedule pt)) per)
             (contains? (:alarms pt) q))}
  {:post (contains? (:quali a) (:quali %))}
  [[a Alarm] [per Period] [pt Plant] -> Expert])
