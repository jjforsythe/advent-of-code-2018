;; Part 1
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input8.txt")))

(defn parse-input [lines]
  (mapv #(Integer/parseInt %) (str/split (get input 0) #" ")))

(defn step [v p sum]
  (if (= (get v p) 0)
    (let [new-sum (+ sum (reduce + (subvec v (+ p 2) (+ (+ p 2) (get v (inc p))))))]
      (if (= p 0) [[] p new-sum]
          [(assoc (vec (concat (subvec v 0 p) (subvec v (+ p 2 (get v (inc p))))))
                  (- p 2) (dec (get v (- p 2))))
           (- p 2) new-sum]
          ))
    (step v (+ p 2) sum)))

(loop [[nodes p sum] [(parse-input input) 0 0]]
  (if (= (count nodes) 0) sum
      (recur (step nodes p sum))))
;; => 42501

;; Part 2
(defn get-node-value [v p current-node nodes]
  (let [values (get-in nodes [current-node :values])
        metadata (subvec v (+ p 2) (+ p 2 (get v (inc p))))]
    (if (= (count (get-in nodes [current-node :values])) 0)
      (reduce + metadata)
      (reduce + (mapv #(if (nil? (get values (dec %))) 0 (get values (dec %)))
                      metadata)))))

(defn step-2 [v p current-node nodes]
  (if (= (get v p) 0)
    [(assoc (vec (concat (subvec v 0 p) (subvec v (+ p 2 (get v (inc p))))))
            (- p 2) (dec (get v (- p 2))))
     (- p 2)
     (:parent (get nodes current-node))
     (assoc-in nodes [(:parent (get nodes current-node)) :values]
               (conj (get-in nodes [(:parent (get nodes current-node)) :values])
                     (get-node-value v p current-node nodes)))]
    (step-2 v (+ p 2) (count nodes)
            (assoc nodes (count nodes) {:parent current-node :values []}))))

(loop [[tree p current-node nodes] [(parse-input input) 0 0 {0 {:values []}}]]
  (if (and (= p 0) (= (get tree 0) 0) )
    (get-node-value tree p current-node nodes)
    (recur (step-2 tree p current-node nodes))))
;; => 30857
