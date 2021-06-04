(ns covering.redblack-tree
  (:require [covering.utils :as utils]))


;; algorithm from the book Purely Functional Data structures
(defn balance [{arg-color :color
                arg-left  :left
                arg-val   :val
                arg-right :right :as tree}]
  (if (= arg-color :black)

   (cond

     ;; L-L
     (and
      (= (:color arg-left) :red)
      (= (get-in arg-left [:left :color]) :red))
     {:color :red
      :val   (:val arg-left)
      :left  (assoc (:left arg-left ):color :black)
      :right {:color :black
              :val   arg-val
              :left  (:right arg-left)
              :right arg-right}}


     ;; L-R
     (and
      (= (:color arg-left) :red)
      (= (get-in arg-left [:right :color]) :red))
     {:color :red
      :val   (get-in arg-left [:right :val])
      :left  (assoc arg-left
                    :color :black
                    :right (get-in arg-left [:right :left]))
      :right {:color :black
              :val   arg-val
              :left  (get-in arg-left [:right :right])
              :right arg-right}}


     ;; R-L
     (and
      (= (:color arg-right) :red)
      (= (get-in arg-right [:left :color]) :red))
     {:color :red
      :val   (get-in arg-right [:left :val])
      :left  {:val   val
              :color :black
              :left  arg-left
              :right (get-in arg-right [:left :left])}
      :right (assoc arg-right
                    :color :black
                    :left (get-in arg-right [:left :right]))}

     ;; R-R
     (and
      (= (:color arg-right) :red)
      (= (get-in arg-right [:right :color]) :red))
     (assoc arg-right
            :left (assoc tree :right (get arg-right :left)))

     :otherwise tree)

  {:val   arg-val
   :color arg-color
   :left  arg-left
   :right arg-right}))



;; modified version of algorithm from Chris Okasaki's Book
(defn insert [event {:keys [val left right color] :as tree} state]
  (cond

    (nil? tree)
    [{:val   event
      :color :red} (conj state event)]

    ;; if disjoint then insert
    (and (utils/disjoint? event val)
         (< (first event) (first val)))
    (let [[t s] (insert event left state)]
      [(balance (assoc tree :left t)) s])


    (and (utils/disjoint? event val)
         (> (first event) (first val)))
    (let [[t s] (insert event right state)]
      [(balance (assoc tree :right t)) s])


    (and (utils/disjoint? event val)
         (= (first event) (first val)))
    [tree (conj state event)]


    :otherwise
    (let [{:keys [events cover-split remainder]} (utils/breaker event val)]
      (reduce
       (fn [[tree state] event]
         (let [[mod-tree mod-state] (insert event tree state)]
           [(assoc mod-tree :color :black)
            mod-state]))

       [(assoc tree :val (first events)) (concat state cover-split)]

       (concat (rest events) (if remainder [remainder] []))))))



(defn insert-into-rb-tree [event rb-tree]
  (let [[tree state] (insert event rb-tree [])]
    [(assoc tree :color :black) state]))
