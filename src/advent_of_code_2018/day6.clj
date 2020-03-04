;; Part 1
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input6.txt")))

(def parsed-input (mapv (fn [x] (mapv #(Integer/parseInt %) (str/split x #", "))) input))

;; Compute bounding box
((juxt #(apply min (map first %))
       #(apply max (map first %))
       #(apply min (map second %))
       #(apply max (map second %))) parsed-input)
;;    [x1 x2  y1 y2]
;; => [53 349 53 357]

(defn rect-cells [left top width height]
  (reduce concat (map #(map (fn [x] (vector % x))
                            (range top (+ top height)))
                      (range left (+ left width)))))

(defn rect-boundary [left top width height]
  (vec (concat (rect-cells left top width 1)
               (rect-cells left top 1 height)
               (rect-cells (dec (+ left width)) top 1 height)
               (rect-cells left (dec (+ top height)) width 1)
               )))

(defn closest-pt [cell]
  (let [mh-dists (mapv #(vector % (mh-dist cell %)) parsed-input)
        close-pt (apply min-key second mh-dists)]
    (if (= (get (frequencies (map second mh-dists)) (second close-pt)) 1)
      (first close-pt)
      ;; Map to an "infinite" cell (hack)
      [66 312])))

(defn mh-dist [[x1 x2] [y1 y2]]
  (+ (Math/abs (- x1 y1)) (Math/abs (- x2 y2))))

(defn map-pts-to-cells [cells]
  (reduce #(let [cp (closest-pt %2)] (assoc %1 cp (inc (get %1 cp))))
          (reduce #(assoc %1 %2 0) {} parsed-input)
          cells))

(def infinite-pts
  (set (mapv first
             (filter #(> (second %) 0) (map-cells
                                        (rect-boundary 53 53 (inc (- 349 53)) (inc (- 357 53))))))))
;; => [[66 312] [53 295] [175 54] [115 53] [79 85] [108 350] [80 357] [73 246] [266 338] [83 192] [95 112] [290 87] [189 322] [349 296] [113 132] [298 342] [348 162] [333 57] [70 243] [161 63] [128 333] [249 90]]

(apply max-key second (filter #(not (get infinite-pts (key %)))
                              (map-pts-to-cells
                               (rect-cells 53 53 (inc (- 349 53)) (inc (- 357 53))))))
;; => [[268 215] 3604]

;; Part 2
(count (filterv (fn [y] (> 10000 y))
                (mapv
                 #(reduce + (mapv (fn [x] (second (vector x (mh-dist % x)))) parsed-input))
                 (rect-cells 53 53 (inc (- 349 53)) (inc (- 357 53)))
                 )))
;; => 46563
