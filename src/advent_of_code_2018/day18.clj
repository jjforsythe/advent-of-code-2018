;; Part 1
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input18.txt")))

(def parsed-map
  (mapv #(str/split % #"") input))

(def acres
  (into {} (map-indexed
            (fn [index line] (reduce-kv #(assoc %1 [%2 index] %3) {} line))
            parsed-map)))

(defn adjacent-acres [[x y]]
  [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
   [(dec x) y] [(inc x) y]
   [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]])

(defn adjacent-types [acre acres]
  (merge {"." 0 "|" 0 "#" 0}
         (frequencies (map #(acres %) (adjacent-acres acre)))))

(defn transform-acre [acre acres]
  (let [acre-type (acres acre)
        types (adjacent-types acre acres)]
    (cond (= acre-type ".")
          (if (>= (types "|") 3) "|" ".")
          (= acre-type "|")
          (if (>= (types "#") 3) "#" "|")
          (= acre-type "#")
          (if (and (>= (types "#") 1)
                   (>= (types "|") 1)) "#" "."))))

(defn tick [acres]
  (reduce #(assoc %1 %2 (transform-acre %2 acres)) {} (keys acres)))

(reduce * ((juxt #(% "|") #(% "#"))
           (frequencies (vals (last (take (inc 10) (iterate tick acres)))))))
;; => 511000

;; Part 2
(defn acres-after-mins [mins]
  (last (take (inc mins) (iterate tick acres))))

(defn frequencies-after-mins [mins]
  (frequencies (vals (acres-after-mins mins))))

;; Assume we are in a loop by 1000 minutes
(frequencies-after-mins 1000)
;; => {"|" 613, "." 1569, "#" 318}

;; Confirm there is a future time with matching frequencies
(+ 1000 1 (count (take-while #(not= {"|" 613, "." 1569, "#" 318} (frequencies (vals %)))
                             (iterate tick (acres-after-mins 1001)))))
;; => 1028

;; Therefore the time period of the cycle is 28 minutes and note that
(= (mod 1000000000 28)
   (mod 1000 28)
   20)

;; Therefore the result is
(reduce * ((juxt #(% "|") #(% "#"))
           (frequencies-after-mins 1000)))
;; => 194934
