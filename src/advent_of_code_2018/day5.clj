;; Part 1
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input5.txt")))

(defn char-test [a b]
  (if (nil? a) false
      (or (and (not= a (str/upper-case a)) (= b (str/upper-case a)))
          (and (not= a (str/lower-case a)) (= b (str/lower-case a)))
          )))

(defn react [s]
  (reduce #(if (char-test (last %1) %2)
             (if (= (count %1) 0) [] (pop %1))
             (conj %1 %2)) [] s))

(def reacted-input (react (str/split (get input 0) #"")))
(count reacted-input)
;; => 10384

;; Part 2
(apply min
       (map (fn [x] (count (react(filter #(and (not= % x) (not= % (str/upper-case x)))
                                         reacted-input)))) (str/split "abcdefghijklmnopqrstuvwxyz" #"")))
;; => 5412
