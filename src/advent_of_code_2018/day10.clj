;; Part 1
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input10.txt")))
(def test-input (str/split-lines (slurp "./src/advent_of_code_2018/test-input10.txt")))

(defn parse-input [lines]
  (mapv #((juxt (fn [x] (subvec x 0 2))
                (fn [x] (subvec x 2 4)))
          (mapv (fn [x] (Integer/parseInt x)) (vec (re-seq #"-?[0-9]\d*" %)))) lines))

((juxt #(apply min %) #(apply max %))
 (map #((% 0) 0) (parse-input input)))
;; => [-53037 53469]
((juxt #(apply min %) #(apply max %))
 (map #((% 0) 1) (parse-input input)))
;; => [-53086 53373]

(defn move-point [p t]
  (apply mapv +
         (update p 1 #(mapv (fn [x] (* x t)) %))))

(defn points-to-grid [points t width height left top scale-factor]
  (map #(apply str (conj % "\n"))
       (reduce #(assoc-in %1 [ (+ top (get %2 1)) (+ left (get %2 0))] "#")
               (vec (repeat height (vec (take width (repeat ".")))))
               (map #(mapv (fn [y] (int (Math/floor (/ y scale-factor)))) (move-point % t))
                    points))))

;; Print function to work with CIDER's cider-pprint-eval-defun-at-point
;; which displays the result in a popup buffer in Emacs.
(defn print-grid [grid]
  (map #(do
          (print (reduce str grid))
          %) [()]))

;; Adjusting the arguments to points-to-grid obtains the result
(print-grid (points-to-grid (parse-input input) 0 110 110 55 55 1000))

;; Appropriate values are in the call below
(print-grid (points-to-grid (parse-input input) 10645 100 30 -165 -130 1))

;; This demonstrates the test data provided with the problem
(print-grid (points-to-grid (parse-input test-input) 3 22 16 6 4 1))
