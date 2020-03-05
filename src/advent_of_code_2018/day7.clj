;; Part 1
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input7.txt")))

(defn parse-input [lines]
  (let [pairs (map #((juxt (fn [x] (re-find #"(?<=Step )." x))
                           (fn [x](re-find #"(?<=step )." x))) %)
                   lines)]
    (reduce #(assoc %1 (second %2)
                    (if (some? (get %1 (second %2)))
                      (conj (get %1 (second %2)) (first %2)) #{(first %2)}))
            (reduce #(assoc %1 %2 #{}) {} (set (mapv first pairs)))
            pairs)))

;; Could (use '[clojure.algo.generic.functor :only (fmap)])
(defn fn-on-values [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn next-instruction [rules]
  (let [instruction (first (vec (sort (mapv first (filter #(= 0 (count (val %))) rules)))))]
    [(identity instruction)
     (fn-on-values (fn [x] (disj x instruction)) (dissoc rules instruction))]
    ))

(defn instruction-sequence [[order rules]]
  (if (> (count rules) 0)
    (instruction-sequence
     ((juxt #(conj order (first %)) second) (next-instruction rules)))
    order))

(apply str (instruction-sequence [[] (parse-input input)]))
;; => "ACHOQRXSEKUGMYIWDZLNBFTJVP"

;; Part 2
(defn instruction-time [instruction]
  (- (int (.charAt instruction 0)) 4))

(defn available-instructions [rules]
  (vec (sort (mapv first (filter #(= 0 (count (val %))) rules)))))

(defn complete-instructions [rules instructions]
  (fn-on-values (fn [x] (apply disj x instructions)) rules))

(dec (loop [t 0
            workers {}
            instructions (parse-input input)]
       (if (or (= t 2000) (and (= (count workers) 0) (= (count instructions) 0))) t
           (let [completed-instructions (mapv first (filterv #(= (val %) 0)
                                                             (fn-on-values dec workers)))
                 free-workers (+ (count completed-instructions ) (- 5 (count workers)))
                 new-instructions (complete-instructions instructions completed-instructions)
                 free-instructions (available-instructions new-instructions)
                 new-worker-instructions (subvec free-instructions 0
                                                 (min free-workers (count free-instructions)))
                 new-worker-times (map instruction-time new-worker-instructions)
                 new-workers (apply hash-map (apply concat (filterv #(> (val %) 0)
                                                                    (fn-on-values dec workers))))]
             (recur (inc t)
                    (if (> (count new-worker-instructions) 0)
                      (apply assoc new-workers
                             (vec (apply concat
                                         (mapv vector new-worker-instructions new-worker-times))))
                      new-workers)
                    (apply dissoc new-instructions new-worker-instructions))))))
;; => 985
