(ns covering.utils)

(defn breaker [[a1 b1 :as event] [a2 b2 :as cover]]
  (cond

    ;; special -cases where extra fragments are added
    ;; duplicate case covered here
    ;(= event cover)
    ;{:events      [cover]
    ; :cover-split {}
    ; :remainder   nil}


    ;(<= b1 a2)
    ;{:events      [event cover]
    ; :cover-split {}
    ; :remainder   nil}


    ;(<= b2 a1)
    ;{:events      [cover]
    ; :cover-split {}
    ; :skip?       true
    ; :remainder   event}


    ;; =================================================


    (and (= a1 a2) (< b1 b2))
    {:events      [:cover-split]
     :cover-split {cover [[a1 b1] [b1 b2]]}
     :remainder   nil}


    (= a1 a2)
    {:events      [:cover-split]
     :cover-split {cover cover}   ; hypothesis: this isn't required
     :remainder   [b2 b1]}


    (and (= b1 b2) (< a1 a2))
    {:events      [[a1 a2] :cover-split]
     :cover-split {cover cover}   ; hypothesis: this isn't required
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
    {:events      [[a1 a2] :cover-split]
     :cover-split {cover cover}
     :remainder   [b2 b1]}))


(defn breaker2 [[a1 b1 :as event] [a2 b2 :as cover]]
  (cond


    (and (= a1 a2) (< b1 b2))
    {:events      [[a1 b1] [b1 b2]]
     :cover-split [{cover [[a1 b1] [b1 b2]]}]
     :remainder   nil}


    (= a1 a2)
    {:events      [cover]
     :cover-split [{cover cover}]   ; hypothesis: this isn't required
     :remainder   [b2 b1]}


    (and (= b1 b2) (< a1 a2))
    {:events      [[a1 a2] cover]
     :cover-split [[a1 a2] {cover cover}]   ; hypothesis: this isn't required
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
     :cover-split [[a1 a2] {cover cover}]
     :remainder   [b2 b1]}))
