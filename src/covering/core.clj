(ns covering.core
  (:require [covering.redblack-tree :as rb-tree]))

;; It is assumed that the sequence is an infinite one spread across time
;; and the events themselves are modelled as start and end points of a subsequence of N

(defn update-mapping [fragment-list mapping-ds event]
  (reduce (fn [acc fragment]
            (if-not (map? fragment)
              (let [events (get acc fragment)]
                (assoc acc fragment (or (and events (concat events [event]))
                                        [event])))
              (let [[k v]  (first fragment)
                    events (get acc k)]
                (reduce (fn [acc_ fragment]
                          (-> acc_
                              (dissoc k)
                              (assoc fragment (cond-> events
                                                (= event fragment) (conj event)))))
                        acc v))))
          mapping-ds fragment-list))


(defn overlapping-events
  ([list-of-events]
   (overlapping-events list-of-events nil {}))

  ([list-of-events covering-ds mapping]
   (reduce
    (fn [[covering_ map_] event]
      (let [[covering fl] (rb-tree/insert-into-rb-tree event covering_)]
        (prn (distinct fl))
        [covering (update-mapping (distinct fl) map_ event)]))

    [covering-ds mapping] list-of-events)))
