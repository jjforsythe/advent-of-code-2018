;; Part 1
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input23.txt")))

(def parsed-input
  (mapv #((juxt (comp vec butlast) last)
          (map (fn [x] (Integer/parseInt x)) (re-seq #"-?\d+" %))) input))

(def strongest-nanobot (apply max-key second parsed-input))

(defn mh-dist [[x1 y1 z1] [x2 y2 z2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2)) (Math/abs (- z1 z2))))

(defn nanobots-in-range [nanobot]
  (count (filter identity
                 (map #(<= (mh-dist (% 0) (nanobot 0)) (nanobot 1)) parsed-input))))

(nanobots-in-range strongest-nanobot)
;; => 305

;; Part 2
(defn count-in-range [pos]
  (count (filter identity
                 (map #(<= (mh-dist (% 0) pos) (% 1)) parsed-input))))

(defn box [[x y z] d]
  [[(+ x d) (+ y d) (+ z d)]
   [(- x d) (+ y d) (+ z d)]
   [(+ x d) (- y d) (+ z d)]
   [(+ x d) (+ y d) (- z d)]
   [x y z]
   [(- x d) (- y d) (+ z d)]
   [(- x d) (+ y d) (- z d)]
   [(+ x d) (- y d) (- z d)]
   [(- x d) (- y d) (- z d)]])

(defn box-2 [[x y z] d]
  [[(+ x d) y z] [(- x d) y z] [x (+ y d) z]
   [x (- y d) z] [x y (+ z d)] [x y (- z d)]
   [(- x d) (- y d) z] [(- x d) y (- z d)] [x (- y d) (- z d)]
   [(- x d) (+ y d) z] [(+ x d) (- y d) z] [(- x d) y (+ z d)]
   [(+ x d) y (- z d)] [x (- y d) (+ z d)] [x (+ y d) (- z d)]
   [(+ x d) (+ y d) z] [(+ x d) y (+ z d)] [x (+ y d) (+ z d)]
   [(+ x d) (+ y d) (+ z d)] [(- x d) (+ y d) (+ z d)] [(+ x d) (- y d) (+ z d)]
   [(+ x d) (+ y d) (- z d)]
   [x y z]
   [(- x d) (- y d) (+ z d)]
   [(- x d) (+ y d) (- z d)] [(+ x d) (- y d) (- z d)] [(- x d) (- y d) (- z d)]])

(defn sample-search [[initial-pos f]]
  (loop [pos initial-pos
         r 30000000
         diff 10000000]
    (let [new-pos (apply max-key count-in-range
                         ((resolve f) pos r))]
      (if (or (< r 2) (not= new-pos pos)) [new-pos f]
          (recur new-pos (- r diff) (if (= (- r diff) diff) (int (/ diff 10)) diff))))))

;; Use a box with 9 vertices (takes ~2 mins to run)
;; Converges to point [22520422 27621795 28545499], in range of 893 nanobots
(->> (first (last (take 22 (iterate sample-search [[0 0 0] 'box]))))
     (reduce +))
;; => 78687716

;; Use a box with 27 vertices (takes ~3.5 mins to run)
;; Converges to point [22512464 28487716 27687536], in range of 895 nanobots
(->> (first (last (take 15 (iterate sample-search [[0 0 0] 'box-2]))))
     (reduce +))
;; => 78687716

;; Other positions at same distance (78687716) from origin
;; In range of 893 nanobots:
;; [22500368 27700028 28487320]
;; [22500376 27700024 28487316]
;; In range of 892 nanobots:
;; [22712524 27287716 28687476]
