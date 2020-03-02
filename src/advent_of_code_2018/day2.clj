;; Part 1
(require '[clojure.string :as str])

(def input (vec (str/split (slurp "./src/advent_of_code_2018/input2.txt") #"\n")))

(defn check-multi-char [s n]
  (if (> (count (filter #(= (second %) n)
                        (frequencies s))) 0)
    1 0))

(* (reduce + (map #(check-multi-char % 3) input))
   (reduce + (map #(check-multi-char % 2) input)))

;; Part 2
(defn str-remove
  "remove elem in coll"
  [pos coll]
  (str (subs coll 0 pos) (subs coll (inc pos))))

(get (nth
      (loop [index 0]
        (if (= 1 (check-multi-char (map #(str-remove index %) input) 2))
          (filter #(= (second %) 2)
                  (frequencies (map #(str-remove 7 %) input)))
          (recur (+ index 1))
          ))
      0) 0)
;; => "mbruvapghxlzycbhmfqjonsie"

;; Character removed in string is at index 7
