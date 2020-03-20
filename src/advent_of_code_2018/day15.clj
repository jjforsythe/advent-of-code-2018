;; Part 1
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input15.txt")))

(def parsed-input (mapv #(str/split % #"") input))

(defn reduce-row [row index m]
  (reduce-kv #(if (= %3 ".") (conj %1 [%2 index]) %1) m row))

(def caverns (reduce-kv (fn [m index val] (reduce-row val index m)) #{} parsed-input))

(def units (reduce-kv (fn [m index val]
                        (reduce-kv
                         #(if (contains? #{"G" "E"} %3) (assoc %1 [%2 index] [%3 200]) %1)
                         m val))
                      {} parsed-input))

(def elves-attack-power 3)

;; This does not account for going out of bounds
(defn get-neighbours [[x y]]
  [[x (dec y)]
   [(dec x) y]
   [(inc x) y]
   [x (inc y)]])

(defn cavern-distance [coord dist m free-caverns free-neighbours-list]
  (let [neighbours (get-neighbours coord)
        free-neighbours (filter free-caverns neighbours)]
    [(reduce #(assoc %1 %2 [coord dist]) m
             free-neighbours)
     (apply disj free-caverns neighbours)
     (apply conj free-neighbours-list free-neighbours)]))

(defn assign-cavern-distances [caverns-at-distance dist m free-caverns]
  (reduce #(cavern-distance %2 dist (%1 0) (%1 1) (%1 2))
          [m free-caverns []]
          caverns-at-distance))

(defn distances-from-cavern [caverns cavern]
  (loop [dist 1
         [m free-caverns edge-caverns] [{cavern [[] 0]} (disj caverns cavern) [cavern]]]
    (if (= 0 (count edge-caverns)) m
        (recur (inc dist) (assign-cavern-distances edge-caverns dist m free-caverns)))))

(defn reachable-caverns [caverns units cavern]
  (reduce
   #(into %1 (remove (comp nil? second)
                     (map (fn [x] (vector x ((distances-from-cavern caverns cavern) x)))
                          (get-neighbours (first %2)))))
   []
   (filter #(= (if (= (first (units cavern)) "E") "G" "E")
               (first (val %)))
           units)))

(defn choose-target [caverns units unit]
  (let [rcs (reachable-caverns caverns units unit)
        min-dist (apply min (map (comp second second) rcs))]
    ((comp first first)
     (sort-by
      #((juxt second first) (first %)) (filter #(= ((comp second second) %) min-dist) rcs)))))

;; think this needs fixing due to only being able to pick up a
;; move from one of the four neighbours
(defn next-move [caverns units unit]
  (let [distances (distances-from-cavern (conj caverns unit) (choose-target caverns units unit))
        distance (dec (second (get distances unit)))
        available-neighbours (filter #(some? (distances %)) (get-neighbours unit))
        options (map first (filter #(= (second (second %)) distance)
                                   (map #(vector % (distances %)) available-neighbours)))
        sorted-options (sort-by (juxt second first) options)]
    (first sorted-options)))

(defn move-unit [caverns units unit]
  (let [position (next-move caverns units unit)]
    [(conj (disj caverns position) unit)
     (set/rename-keys units {unit position})
     position]))

;; hopefully this returns the first in reading order as the neighbours
;; should be in reading order
(defn select-target [caverns units unit]
  (let [possible-targets (->> unit
                              (get-neighbours)
                              (map (set (keys units)))
                              (remove nil?)
                              (filter #(= (first (units %))
                                          (if (= (first (units unit)) "E") "G" "E"))))]
    (if (= 0 (count possible-targets)) []
        (let [min-hit-points (apply min (map #(second (units %)) possible-targets))
              targets-with-min-hit-points (filter #(= min-hit-points (second (units %)))
                                                  possible-targets)]
          (first targets-with-min-hit-points)))))

(defn target-in-range? [caverns units unit]
  (->> unit
       (select-target caverns units)
       (count)
       (< 0)))

(defn attack [caverns units unit]
  (let [target (select-target caverns units unit)
        elf? (= "E" (first (units unit)))
        units-after-attack (update-in units [target 1] #(- % (if elf? elves-attack-power 3)))
        units-killed? (<= ((units-after-attack target) 1) 0)
        units-killed-removed (if units-killed?
                               (dissoc units-after-attack target) units-after-attack)
        caverns-after-attack (if units-killed? (conj caverns target) caverns)]
    [caverns-after-attack
     units-killed-removed]))

(defn move-and-attack [caverns units unit]
  (let [[caverns units position] (move-unit caverns units unit)]
    (if (target-in-range? caverns units position)
      (attack caverns units position)
      [caverns units])))

;; from Stack Overflow
(defn take-while+
  [pred coll]
  (lazy-seq
   (when-let [[f & r] (seq coll)]
     (if (pred f)
       (cons f (take-while+ pred r))
       [f]))))

(defn round [[caverns units]]
  (let [unit-positions (sort-by (juxt second first) (keys units))]
    (loop [index 0
           [caverns units] [caverns units]]
      (if (or (= index (count unit-positions))
              (<= (count (set (map (comp first second) units))) 1)) [caverns units]
          (recur (inc index)
                 (let [unit (nth unit-positions index)]
                   (if (some? (units unit))
                     (if (target-in-range? caverns units unit)
                       (attack caverns units unit)
                       (if (< 0 (count (reachable-caverns caverns units unit)))
                         (move-and-attack caverns units unit)
                         [caverns units]))
                     [caverns units])))))))

(defn run-combat []
  ((juxt (comp dec count) (comp #(reduce + (map (comp second val) %)) second last)
         (comp second last))
   (take-while+ #(> (count (set (map (comp first second) (second %)))) 1)
                (iterate round [caverns units]))))

;; Do single round
(time (count (round [caverns units])))

;; Do n rounds
(let [rounds 3] (last (take (inc rounds) (iterate round [caverns units]))))

;; Compute outcome with assumption that the last round was not a full round
;; This should be run with
;; (def elves-attack-power 3)
(let [combat-result (run-combat)]
  (* (dec (combat-result 0)) (combat-result 1)))
;; => 216270

;; Part 2
;; Tested with different values to conclude that 15 is minimal
(def elves-attack-power 15)

(let [combat-result (run-combat)]
  (* (dec (combat-result 0)) (combat-result 1)))
;; => 59339
