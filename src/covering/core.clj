(ns covering.core
  (:require [covering.redblack-tree :as rb-tree]))

;; It is assumed that the sequence is an infinite one spread across time
;; and the events themselves are modelled as start and end points of a subsequence of N


;; type Event = (Int, Int)
;; type MapDS = Map Event [Event]
;; :: [(Either Event MapDS)] -> MapDS -> Event -> MapDS
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
