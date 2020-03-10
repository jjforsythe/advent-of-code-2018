;; Part 1
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input12.txt")))

(def initial-state (str (re-find #"(?<=initial state: ).*" (get input 0))))

(def transitions (reduce #(assoc %1 (subs %2 0 5) (str (last %2))) {} (subvec input 2)))

(defn get-neighbours [state index]
  (str/replace (format "%5s"
                       (subs (str state "...")
                             (max 0 (- index 2))
                             (min (count (str state "...")) (+ index 3)))) #" " "."))

(defn next-state [[state p]]
  (let [new-state (reduce str (map #(get transitions (get-neighbours state %) ".")
                                   (range -1 (inc (count state)))))]
    [(re-find #"#.*#" new-state) (if (= (first new-state) \#) (dec p)
                                     (if (= (subs new-state 0 4) "....") (+ 3 p)
                                         (if (= (subs new-state 0 3) "...") (+ 2 p)
                                             (if (= (subs new-state 0 2) "..")
                                               (+ p 1) p))))]))

(let [final-state (last (take (inc 20) (iterate next-state [initial-state 0])))]
  (reduce + (map-indexed #(if (= %2 "#") (+ %1 (second final-state)) 0)
                         (str/split (first final-state) #""))))
;; => 1696

;; Part 2
;; After 200 generations
;; => 6658

;; Therefore after 50000000000 generations (there are 36 pots in the pattern which moves in the
;; positive direction by one every generation)
(+ 6658 (* (- 50000000000 200) 36))
;; => 1799999999458
