(ns covering.core)

;; It is assumed that the sequence is an infinite one spread across time
;; and the events themselves are modelled as start and end points of a subsequence of N


;; :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
;(defn insert-into-covering [covering event]
;  (loop [result        []              ;; [Event]
;         fragment-list []              ;; [Either Event (Map Event [Event])]
;         covering_     covering        ;; [Event]
;         event_        event]          ;; Event
;    (prn "event is " event_)
;    (prn "covering is " covering_)
;    (cond
;      (empty? event_)     (do
;                            (prn "This is executed")
;                            (prn "-> " (concat result covering_))
;                            [fragment-list  (concat result covering_)])
;      (empty? covering_)   [(concat fragment-list [event_]) (concat result [event_])] ;; TODO put in map here
;
;      :else             (let [cover
;                              (first covering_)
;
;                              {:keys [events cover-split remainder skip?]}
;                              (breaker cover event)]
;
;                          (recur (concat result (util events :cover-split (get cover-split cover)))
;
;                                 (if skip?
;                                   fragment-list
;                                   (concat fragment-list (util events :cover-split cover-split)))
;
;                                 (rest covering_)
;
;                                 remainder
;                                 )))))



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
                              (assoc fragment (conj events event))))
                        acc v))))
          mapping-ds fragment-list))


(defn overlapping-events
  ([list-of-events]
   (overlapping-events list-of-events nil {}))

  ([list-of-events covering-ds mapping]
   (reduce (fn [[covering_ map_] event]
             (let [[covering fl] (insert-into-rb-tree event covering_)]
               [covering
                (update-mapping fl map_ event)]))
           [covering-ds mapping] list-of-events)))
