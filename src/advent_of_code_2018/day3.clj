;; Part 1
(require '[clojure.string :as str])

(def input (vec (str/split (slurp "./src/advent_of_code_2018/input3.txt") #"\n")))

(defn parse-input [s]
  (mapv #(Integer/parseInt %) (mapv (str/split s #"x|,|:| ") [2 3 5 6])))

(defn rect-cells [left top width height]
  (reduce concat (map #(map (fn [x] (vector % x))
                            (range top (+ top height)))
                      (range left (+ left width)))))

(count (filter #(> (second %) 1)
               (frequencies
                (reduce concat
                        (map #(apply rect-cells (parse-input %)) input)))))
;; => 110383

;; Part 2
;; Compare each rectangle to see if it has any squares in the list from part 1
(def overlaps (map first (filter #(> (second %) 1)
                                 (frequencies
                                  (reduce concat
                                          (map #(apply rect-cells (parse-input %)) input))))))

(keep-indexed #(if (= false %2) %1)
              (map #(some? (some (set (apply rect-cells (parse-input %))) overlaps)) input))
;; => (128)
;; Answer is 129 as the IDs are 1-indexed rather than 0-indexed
