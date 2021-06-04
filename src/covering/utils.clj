(ns covering.utils)


(defn disjoint? [[a1 b1] [a2 b2]]
  (or (and (<= b1 a2)
           (< a1 a2)
           (< b1 b2))
      (and (< a2 a1)
           (<= b2 a1)
           (< b2 b1))))


(defn breaker [[a1 b1 :as event] [a2 b2 :as cover]]
  (cond


    (and (= a1 a2) (< b1 b2))
    {:events      [[a1 b1] [b1 b2]]
     :cover-split [{cover [[a1 b1] [b1 b2]]}]
     :remainder   nil}


    (= a1 a2)
    {:events      [cover]
     :cover-split [{cover [cover]}]   ; hypothesis: this isn't required
     :remainder   [b2 b1]}


    (and (= b1 b2) (< a1 a2))
    {:events      [[a1 a2] cover]
     :cover-split [[a1 a2] {cover [cover]}]   ; hypothesis: this isn't required
     :remainder   nil}

    (= b1 b2)
    {:events      [[a2 a1] [a1 b1]]
     :cover-split [{cover [[a2 a1] [a1 b1]]}]
     :remainder   nil}


    ;;      a1------b1
    ;;   a2----------------b2
    ;;
    (and (> a1 a2) (< b1 b2))
    {:events       [[a2 a1] [a1 b1] [b1 b2]]
     :cover-split  [{cover [[a2 a1] [a1 b1] [b1 b2]]}]
     :remainder    nil}

    ;;       a1--------------b1
    ;;   a2--------b2
    ;;
    (> a1 a2)
    {:events       [[a2 a1] [a1 b2]]
     :cover-split  [{cover [[a2 a1] [a1 b2]]}]
     :remainder    [b2 b1]}


    ;;  a1 ---------- b1
    ;;      a2 -------------b2
    ;;
    (and (< a1 a2) (< b1 b2))
    {:events      [[a1 a2] [a2 b1] [b1 b2]]
     :cover-split [[a1 a2] {cover [[a2 b1] [b1 b2]]}]
     :remainder   nil}

    ;;  a1 ----------- b1
    ;;       a2---b2
    ;;
    :else
    {:events      [[a1 a2] cover]
     :cover-split [[a1 a2] {cover [cover]}]
     :remainder   [b2 b1]}))
