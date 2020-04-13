;; Part 1
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input25.txt")))

(defn parse-input []
  (mapv #(mapv (fn [x] (Integer/parseInt x)) (re-seq #"-?\d+" %)) input))

(defn mh-dist [x y]
  (reduce #(+ %1 (Math/abs %2)) 0 (map - x y)))

(defn within-cluster [cluster p]
  (some true? (map #(<= (mh-dist p %) 3) cluster)))

(defn assign-cluster [clusters p]
  (let [within-clusters (reduce-kv #(if (within-cluster %3 p) (conj %1 %2) %1) [] clusters)]
    (cond (= (count within-clusters) 0) (conj clusters [p])
          (= (count within-clusters) 1)
          (assoc clusters (first within-clusters) (conj (clusters (first within-clusters)) p))
          :else (reduce #(apply assoc %1 %2)
                        (assoc clusters (first within-clusters)
                               (reduce #(apply conj %1 %2)
                                       (conj (clusters (first within-clusters)) p)
                                       (map clusters (rest within-clusters))))
                        (map vector (rest within-clusters) (repeat (count within-clusters) []))))))

(defn constellations [points]
  (reduce #(assign-cluster %1 %2) [[(points 0)]] (rest points)))

(time (count (filter #(not= % []) (constellations (parse-input)))))
;; => 331
