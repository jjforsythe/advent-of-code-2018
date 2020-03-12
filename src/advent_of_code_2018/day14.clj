;; Part 1
(require '[clojure.string :as str])

(defn tick [[recipes e1 e2]]
  (let [sum-recipes (+ (recipes e1) (recipes e2))
        updated-recipes (vec (apply conj recipes
                                    (if (>= sum-recipes 10)
                                      [(int (/ sum-recipes 10))
                                       (mod sum-recipes 10)]
                                      [(mod sum-recipes 10)])))]
    [updated-recipes
     (mod (+ e1 1 (recipes e1)) (count updated-recipes))
     (mod (+ e2 1 (recipes e2)) (count updated-recipes))]))

(let [num-recipes 824501
      final-state (last (take-while #(< (count (first %)) (+ 2 10 num-recipes))
                                    (iterate tick [[3 7] 0 1])))]
  (apply str (subvec (first final-state) num-recipes (+ num-recipes 10))))
;; => "1031816654"

;; Part 2
(let [input [8 2 4 5 0 1]
      final-state (last (take-while
                         (fn [state]
                           (every? identity
                                   ((juxt #(not= (pop %) input)
                                          #(not= (rest %) input))
                                    (subvec (first state)
                                            (- (count (first state)) (count input) 2)
                                            (- (count (first state)) 1)))))
                         (iterate tick [[3 7 1 0 1 0 1 2] 0 6])))
      final-recipes (first final-state)
      recipes-if-at-end (- (count final-recipes) (count input))]
  (if (= (subvec final-recipes recipes-if-at-end) input)
    recipes-if-at-end
    (inc recipes-if-at-end)))
;; => 20179839
