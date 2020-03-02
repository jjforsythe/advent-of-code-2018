;; Part 1
(require '[clojure.string :as str])

(def input (mapv #(Integer/parseInt %)
                 (str/split (slurp "./src/advent_of_code_2018/input1.txt") #"\n")))

(reduce + input)
;; => 540

;; Part 2
(defn find-duplicate-freq [frequency-changes]
  (loop [freq-map {}
         current-freq 0
         index 0]
    (if (contains? freq-map current-freq)
      current-freq
      (recur (assoc freq-map current-freq "")
             (+ current-freq (get frequency-changes (mod index (count frequency-changes))))
             (inc index)))))

(find-duplicate-freq input)
;; => 73056
