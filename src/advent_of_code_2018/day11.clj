;; Part 1
(require '[clojure.string :as str])

(defn hundreds-digit [x]
  (mod (int (Math/floor (/ x 100))) 10))

(defn power-level [x y serial-number]
  (let [rack-id (+ x 10)]
    (- (hundreds-digit (* (+ (* rack-id y) serial-number) rack-id)) 5)))

(power-level 3 5 8)
;; => 4
(power-level 122 79 57)
;; => -5
(power-level 217 196 39)
;; => 0
(power-level 101 153 71)
;; => 4

(defn rect-cells [left top width height]
  (vec (reduce concat (map
                       #(map (fn [x] (vector % x)) (range top (+ top height)))
                       (range left (+ left width))))))

(apply max-key #(get % 2) (mapv (fn [coord]
                                  (conj coord
                                        (reduce + (mapv
                                                   #(power-level (first %) (second %) 7803)
                                                   (rect-cells (first coord) (second coord) 3 3)))))
                                (rect-cells 1 1 298 298)))
;; => [20 51 31]
;; Part Two
(def power-levels (mapv #(power-level (first %) (second %) 7803) (rect-cells 1 1 300 300)))

(defn cells [[x y] n]
  (+ (reduce + (mapv #(get power-levels (+ (* (- %1 1) 300) %2 -1))
                     (repeat n (+ x n -1))
                     (range y (+ y n))))
     (reduce + (mapv #(get power-levels (+ (* (- %1 1) 300) %2 -1))
                     (range x (+ x (dec n)))
                     (repeat (dec n) (+ y n -1))))))

;; This takes ~10 mins to run
;; Commenting out the (rect-cells) line and replacing with
;; the commmented line below runs quicker by setting the
;; top-left corner to be 230,272
(time (apply max-key last
             (map
              (fn [[x y]]
                (loop [n 1
                       powers [0]]
                  (if (= n (min (- 302 x) (- 302 y)))
                    (concat [x y] (update (apply max-key second
                                                 (map-indexed vector (rest powers))) 0 inc))
                    (recur (inc n) (conj powers (+ (last powers) (cells [x y] n)))))))
              (rect-cells 1 1 300 300)
              ;; [[230 272]]
              )))
;; => (230 272 17 125)
