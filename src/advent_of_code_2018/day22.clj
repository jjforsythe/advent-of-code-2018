;; Part 1
(require '[clojure.set :as set])
(require '[clojure.data.priority-map :refer [priority-map priority-map-keyfn-by]])

(def target [8 701])
(def depth 5913)

;; Test data
;; (def target [10 10])
;; (def depth 510)

(def x-max (+ 300 (target 0)))
(def y-max (+ 300 (target 1)))

(defn geologic-index [geo-indexes [x y]]
  (cond (= [x y] [0 0]) 0
        (= [x y] target) 0
        (= y 0) (* x 16807)
        (= x 0) (* y 7905)
        :else (* (geo-indexes [(dec x) y]) (geo-indexes [x (dec y)]))))

(defn erosion-levels [v]
  (reduce #(assoc %1 %2 (-> (geologic-index %1 %2)
                            (+ depth)
                            (mod 20183))) {} v))

(defn types [e-levels]
  (reduce #(assoc %1 (%2 0)
                  (let [el-mod-3 (mod (%2 1) 3)]
                    (cond (= el-mod-3 0) "."
                          (= el-mod-3 1) "="
                          (= el-mod-3 2) "|")))
          {} e-levels))

(defn rect-cells [left top width height]
  (reduce concat (map #(map (fn [x] (vector % x))
                            (range top (+ top height)))
                      (range left (+ left width)))))

(reduce #(cond (= (%2 1) "=") (inc %1)
               (= (%2 1) "|") (+ %1 2)
               :else %1)
        0
        (merge (-> (rect-cells 0 0 (inc (target 0)) (inc (target 1)))
                   (erosion-levels)
                   (types))))
;; => 6256

;; Part 2
(def regions (merge (-> (rect-cells 0 0 (inc x-max) (inc y-max))
                        (erosion-levels)
                        (types))))

(defn move-cost [current-pos next-pos types]
  (let [next-type (regions next-pos)]
    (if (types next-type) [1 types]
        [8 (apply disj #{"." "=" "|"} (vec (disj types (regions current-pos))))])))

(defn neighbours [[x y]]
  (->> [[x (dec y)]
        [(dec x) y]
        [(inc x) y]
        [x (inc y)]]
       (filter #(and (>= (% 0) 0) (>= (% 1) 0)))
       (filter #(and (<= (% 0) x-max) (<= (% 1) y-max)))))

(defn dist-to-target [[x y]]
  (+ (Math/abs (- x (target 0))) (Math/abs (- y (target 1)))))

(defn a-star [frontier]
  (let [current-state (peek frontier)
        pos (first (first current-state))]
    (-> (reduce #(let [cost (move-cost pos %2 (get (first current-state) 1))
                       next-pos-cost (+ (cost 0) (second (second current-state)))]
                   (into %1 (-> []
                                ((fn [x] (if (or (not (%1 [%2 (cost 1)]))
                                                 (> ((%1 [%2 (cost 1)]) 1) next-pos-cost))
                                           (conj x [[%2 (cost 1)]
                                                    [(+ (dist-to-target %2) next-pos-cost)
                                                     next-pos-cost]]) x))))))
                frontier
                (neighbours pos))
        (assoc-in [(first current-state) 0] -1))))

(defn a-star-iterate []
  (iterate a-star
           (priority-map-keyfn-by
            first
            #(cond (< %1 0) false (< %2 0) true :else (< %1 %2))
            [[0 0] #{"." "|"}] [(dist-to-target [0 0]) 0])))

(defn take-while+
  [pred coll]
  (lazy-seq
   (when-let [[f & r] (seq coll)]
     (if (pred f)
       (cons f (take-while+ pred r))
       [f]))))

(apply min (map + [7 0]
                (map second (map
                             (last (take-while+
                                    #(or (not= -1 (get (% [target #{"." "="}]) 0))
                                         (not= -1 (get (% [target #{"." "|"}]) 0)))
                                    (a-star-iterate)))
                             [[target #{"." "="}]
                              [target #{"." "|"}]]))))
;; => 973
