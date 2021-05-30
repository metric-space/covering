(ns covering.core)

;; It is assumed that the sequence is an infinite one spread across time
;; and the events themselves are modelled as start and end points of a subsequence of N


;;assumed here a1 is supposed the leading event
;;
;;  case (i)
;;      X---X
;;      X-------X
;;
;;  case (ii)
;;      X-------X
;;      X---X
;;
;;  case (iii)
;;      X--------X
;;         X--X
;;  case (iv)
;;      X--------X
;;          X-------------X
;;
;;   second argument is the covering and the
;;
(defn breaker [[a1 b1 :as event] [a2 b2 :as cover]]
  (cond


    (= event cover)
    {:events      [cover]
     :cover-split {}
     :remainder   nil}


    (<= b1 a2)
    {:events      [event cover]
     :cover-split {}
     :remainder   nil}


    (<= b2 a1)
    {:events      [cover]
     :cover-split {}
     :remainder   event}


    (and (= a1 a2) (< b1 b2))
    {:events      [:cover-split]
     :cover-split {cover [[a1 b1] [b1 b2]]}
     :remainder   nil}


    (= a1 a2)
    {:events      [:cover-split]
     :cover-split {cover cover}
     :remainder   [b2 b1]}


    (and (= b1 b2) (< a1 a2))
    {:events      [[a1 a2] :cover-split]
     :cover-split {cover cover}
     :remainder   nil}

    (= b1 b2)
    {:events      [:cover-split]
     :cover-split {cover [[a2 a1] [a1 b1]]}
     :remainder   nil}


    ;;      a1------b1
    ;;   a2----------------b2
    ;;
    (and (> a1 a2) (< b1 b2))
    {:events       [:cover-split]
     :cover-split  {cover [[a2 a1] [a1 b1] [b1 b2]]}
     :remainder    nil}

    ;;       a1--------------b1
    ;;   a2--------b2
    ;;
    (> a1 a2)
    {:events       [:cover-split]
     :cover-split  {cover [[a2 a1] [a1 b2]]}
     :remainder    [b2 b1]}


    ;;  a1 ---------- b1
    ;;      a2 -------------b2
    ;;
    (and (< a1 a2) (< b1 b2))
    {:events      [[a1 a2] :cover-split]
     :cover-split {cover [[a2 b1] [b1 b2]]}
     :remainder   nil}

    ;;  a1 ----------- b1
    ;;       a2---b2
    ;;
    :else
    {:events      [[a1 a2] cover-split]
     :cover-split {cover cover}
     :remainder   [b2 b1]}))


;; -===== util =============
(defn util [list to-replace replacement]
  (map (fn [x]
         (if (= to-replace x) replacement x)) list))
;; =========================


;; :: [(Int,Int)] -> (Int,Int) -> [(Int,Int)]
(defn insert-into-covering [covering event]
  (loop [result        []              ;; [Event]
         fragment-list []              ;; [Either Event (Map Event [Event])]
         covering_     covering        ;; [Event]
         event_        event]          ;; Event
    (cond
      (empty? covering_)   [fragement-list (concat result [event_])]
      (empty? event)       [fragment-list  (concat result covering_)]

      :else             (let [cover
                              (first covering_)

                              {:keys [events cover-split remainder]}
                              (breaker cover event)]
                          (recur (concat result        (util events :cover-split (get cover-split cover)))
                                 (concat fragment-list (util events :cover-split cover-split))
                                 (rest covering_)
                                 remainder
                                 )))))


(defn overlapping-events
  ([list-of-new-events]
   (overlapping-events list-of-events [] {}))

  ([list-of-new-events covering-ds mapping]
   (reduce (fn [[covering_ map_] event]
             (let [[fl covering] (insert-into-covering covering_ event)]
               (reduce (fn [acc y]
                         (if-not (map? y)
                           (assoc acc y [event])
                           (let [[k v] (first y)
                                 ito   (get map_ k)]
                             (reduce (fn [acc_ fragment]
                                       (assoc acc_ fragment (conj ito event)))
                                     acc v))))
                       mapping fl)))
           [covering-ds mapping] list-of-events)))


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
