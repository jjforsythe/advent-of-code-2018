;; Part 1
(require '[clojure.string :as str])
(require '[clojure.data.priority-map :refer [priority-map-keyfn]])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input24.txt")))

(defn parse-line [line type]
  (assoc {}
         :type type
         :units (Integer/parseInt (re-find #"\d+" line))
         :hit-points (Integer/parseInt (re-find #"(?<=with )\d*" line))
         :damage (Integer/parseInt (re-find #"(?<=does )\d*" line))
         :damage-type (re-find #"(?<= )\w+(?= damage)" line)
         :initiative (Integer/parseInt (re-find #"(?<=initiative )\d*" line))
         :weak (let [weaknesses (get (re-find #"(?<=weak to )(\w+|\,|\s)*" line) 0)]
                 (if weaknesses
                   (-> weaknesses
                       (str/replace " " "")
                       (str/split #",")
                       set)
                   #{}))
         :immune (let [weaknesses (get (re-find #"(?<=immune to )(\w+|\,|\s)*" line) 0)]
                   (if weaknesses
                     (-> weaknesses
                         (str/replace " " "")
                         (str/split #",")
                         set)
                     #{}))))

;; target selection phase
(defn effective-power [group]
  (* (group :units) (group :damage)))

(def groups
  (let [break (first (keep-indexed #(if (= %2 "") %1) input))]
    (->>
     (vec (concat (map #(parse-line % "immune") (subvec input 1 break))
                  (map #(parse-line % "infection") (subvec input (+ break 2)))))
     (reduce-kv #(assoc %1 %2 %3)
                (priority-map-keyfn (juxt :type effective-power :initiative))))))

(defn damage [group-att group-def]
  (cond ((group-def :immune) (group-att :damage-type))
        0
        ((group-def :weak) (group-att :damage-type))
        (* 2 (effective-power group-att))
        :else (effective-power group-att)))

(defn target-selection [groups]
  (let [order (vec (reverse (keys groups)))]
    (loop [index 0
           groups groups
           immune-targets
           (reduce #(if (= ((val %2) :type) "immune") (conj %1 %2) %1)
                   (priority-map-keyfn (juxt effective-power :initiative)) groups)
           infection-targets
           (reduce #(if (= ((val %2) :type) "infection") (conj %1 %2) %1)
                   (priority-map-keyfn (juxt effective-power :initiative)) groups)]
      (if (= index (count order)) groups
          (let [targets (if (= "infection" (get-in groups [(order index) :type]))
                          immune-targets infection-targets)
                sample-target (if (> (count targets) 0)
                                (key (apply max-key
                                            #(damage (groups (order index)) (val %))
                                            targets))
                                nil)
                target (if (and sample-target
                                (not= 0 (damage (groups (order index)) (groups sample-target))))
                         sample-target
                         nil)]
            (if (and (> (count targets) 0)
                     target
                     (= 0 (damage
                           (groups (order index))
                           (groups target)))) (println "whoops") )
            (recur (inc index)
                   (assoc-in groups [(order index) :target] target)
                   (dissoc immune-targets target)
                   (dissoc infection-targets target)))))))

;; attacking phase
(defn attack-order [groups]
  (vec (reverse (keys (into (priority-map-keyfn :initiative) groups)))))

(defn attack [groups]
  (reduce #(if (%1 %2)
             (let [group-att (%1 %2)
                   group-def-index (group-att :target)
                   group-def (%1 group-def-index)]
               (if (nil? group-def) %1
                   (let [units-remaining (max 0 (- (group-def :units)
                                                   (int (/ (damage group-att group-def)
                                                           (group-def :hit-points)))))]
                     (if (= units-remaining 0) (dissoc %1 group-def-index)
                         (assoc-in %1 [group-def-index :units] units-remaining)))))
             %1)
          groups (attack-order groups)))

(defn fight [groups]
  (attack (target-selection groups)))

(defn take-while+
  [pred coll]
  (lazy-seq
   (when-let [[f & r] (seq coll)]
     (if (pred f)
       (cons f (take-while+ pred r))
       [f]))))

(reduce + (map #((val %) :units)
               (last (take-while+
                      #(and (> (count (filter (fn [x] (= "immune" (:type (val x)))) %)) 0)
                            (> (count (filter (fn [x] (= "infection" (:type (val x)))) %)) 0))
                      (iterate fight groups)))))
;; => 19381

;; Part 2
(defn boosted-groups [groups boost]
  (reduce
   #(if (= "immune" ((val %2) :type))
      (assoc %1 (key %2) (update (val %2) :damage (fn [x] (+ x boost))))
      (conj %1 %2))
   (priority-map-keyfn (juxt :type effective-power :initiative))
   groups))

(defn run-with-boost [boost]
  (last (take-while+
         #(and (> (count (filter (fn [x] (= "immune" (:type (val x)))) %)) 0)
               (> (count (filter (fn [x] (= "infection" (:type (val x)))) %)) 0))
         (iterate fight (boosted-groups groups boost)))))

;; Runs into an infinite loop with a boost of 33, 34 is the first win
(reduce + (map #((val %) :units)
               (run-with-boost 34)))
;; => 3045
