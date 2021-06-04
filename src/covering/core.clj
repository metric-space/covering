(ns covering.core
  (:require [covering.redblack-tree :as rb-tree]))

;; It is assumed that the sequence is an infinite one spread across time
;; and the events themselves are modelled as start and end points of a subsequence of N


;; type Event = (Int, Int)
;; type MapDS = Map Event [Event]
;; :: [(Either Event MapDS)] -> MapDS -> Event -> MapDS
;; meaning of output of type MapDS, values are list of overlapping events (if count > 1)
;; and keys are disjoint intersections of overlapping events
(defn update-mapping [fragment-list mapping-ds event]
  (reduce (fn [map-ds fragment]
            (if-not (map? fragment)
              (let [events (get map-ds fragment)]
                (assoc map-ds fragment (or (and events (conj events event))
                                           [event])))
              (let [[k v]  (first fragment)
                    events (get map-ds k)]
                (reduce (fn [acc_ fragment]
                          (-> acc_
                              (dissoc k)
                              (assoc fragment (cond-> events
                                                (= event fragment) (conj event)))))
                        map-ds v))))
          mapping-ds fragment-list))


;;   (Main function)
;;
;; [(Int,Int)] -> (RedBlackTree (Int,Int) , Map (Int,Int) [(Int,Int)])
;; Meaning of the map in  :->: Values are list of overlapping events
;;                             Keys are the intersections of these overlapped events
(defn overlapping-events
  ([list-of-events]
   (overlapping-events list-of-events nil {}))

  ([list-of-events covering-ds mapping]
   (reduce
    (fn [[acc-covering acc-map] event]
      (let [[sorted-covering cover-fragments]
            (rb-tree/insert-into-rb-tree event acc-covering)]

        [sorted-covering
         (update-mapping (distinct cover-fragments) acc-map event)]))

    [covering-ds mapping] list-of-events)))
