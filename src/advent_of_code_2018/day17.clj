;; Part 1
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input17.txt")))

(defn get-range-y [x y1 y2]
  (set (map vector (repeat (inc (- y2 y1)) x) (range y1 (inc y2)))))

(defn get-range-x [y x1 x2]
  (set (map vector (range x1 (inc x2)) (repeat (inc (- x2 x1)) y))))

(defn parse-input [lines]
  (map (fn [line] (if (= (get line 0) \x)
                    (apply get-range-y
                           (map (fn [x] (Integer/parseInt x))
                                ((juxt #(re-find #"(?<=x=)\d*" %)
                                       #(re-find #"(?<=y=)\d*" %)
                                       #(re-find #"(?<=\.\.)\d*" %))
                                 line)))
                    (apply get-range-x
                           (map (fn [x] (Integer/parseInt x))
                                ((juxt #(re-find #"(?<=y=)\d*" %)
                                       #(re-find #"(?<=x=)\d*" %)
                                       #(re-find #"(?<=\.\.)\d*" %))
                                 line)))))
       lines))

(def clay (reduce set/union #{} (parse-input input)))

(def y-max
  ((apply max-key #(% 1) clay) 1))

(def y-min
  ((apply min-key #(% 1) clay) 1))

(def water-spring [500 0])

(defn up [[x y]]
  [x (dec y)])

(defn down [[x y]]
  [x (inc y)])

(defn left [[x y]]
  [(dec x) y])

(defn right [[x y]]
  [(inc x) y])

(defn to-water-map [v]
  (zipmap v (repeat (count v) \|)))

(defn to-settled-water-map [v]
  (zipmap v (repeat (count v) \~)))

(defn down-to-clay [coord water]
  (let [steps (vec (take-while #(and (nil? (clay %)) (nil? (water %))
                                     (<= (% 1) y-max)) (iterate down (down coord))))
        position (if (= 0 (count steps)) coord (last steps))]
    [position (merge water (to-water-map steps))]))

(defn left-to-clay [[coord water]]
  (let [new-steps (vec (take-while #(and (nil? (clay %))
                                         (or (some? (clay (down %)))
                                             (= (water (down %)) \~)))
                                   (iterate left (left coord))))
        steps (if (= 0 (count new-steps)) [coord] new-steps)]
    (let [fill? (some? (clay (left (last steps))))
          drop? (and (not fill?)
                     (nil? (water (left (last steps)))))]
      [(if fill? "FILL"
           (if drop? "DROP" "NO-FILL"))
       (if drop? (conj steps (left (last steps))) steps)])))

(defn right-to-clay [[coord water]]
  (let [new-steps (vec (take-while #(and (nil? (clay %))
                                         (or (some? (clay (down %)))
                                             (= (water (down %)) \~)))
                                   (iterate right (right coord))))
        steps (if (= 0 (count new-steps)) [coord] new-steps)]
    (let [fill? (some? (clay (right (last steps))))
          drop? (and (not fill?)
                     (nil? (water (right (last steps)))))]
      [(if fill? "FILL"
           (if drop? "DROP" "NO-FILL"))
       (if drop? (conj steps (right (last steps))) steps)])))

(defn fill [coord water]
  (let [lc (left-to-clay [coord water])
        rc (right-to-clay [coord water])]
    (if (and (= (lc 0) "FILL") (= (rc 0) "FILL"))
      (let [filled-water (merge (to-settled-water-map (lc 1))
                                (to-settled-water-map (rc 1))
                                {coord \~})]
        (recur (let [water-above (filter #(= (water (up %)) \|) (keys filled-water))]
                 (if (not= 1 (count water-above)) (up coord)
                     (up (first water-above))))
               (merge water filled-water)))
      (let [drop-points (-> []
                            (#(if (and (= (rc 0) "DROP")
                                       (nil? (water (last (rc 1)))))
                                (conj % (last (rc 1))) %))
                            (#(if (and (= (lc 0) "DROP")
                                       (nil? (water (last (lc 1)))))
                                (conj % (last (lc 1))) %)))]
        [drop-points
         (merge {coord \|} (to-water-map (lc 1)) (to-water-map (rc 1)) water)]))))

(defn drop-water [[coord & coords] water]
  (if (nil? coord)
    [(count (filter #(>= (% 1) y-min) (keys water)))
     (count (filter #(= (val %) \~) water))]
    (let [state (down-to-clay coord water)]
      (if (or (= y-max (get-in state [0 1]))
              (= coord (get state 0))
              (= \| ((get state 1) (down (get state 0)))))
        (recur coords (state 1))
        (let [filled-coord (apply fill state)]
          (recur (apply conj coords (filled-coord 0)) (filled-coord 1)))))))

((drop-water [water-spring] {}) 0)
;; => 30384

;; Part 2
((drop-water [water-spring] {}) 1)
;; => 24479
