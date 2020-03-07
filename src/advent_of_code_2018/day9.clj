;; Part 1
(defn insert-marble [marbles p next-marble scores]
  (if (not= (mod next-marble 23) 0)
    [(-> marbles
         (assoc-in [((marbles p) 0) 0] next-marble)
         (conj [((marbles ((marbles p) 0)) 0) ((marbles p) 0)])
         (assoc-in [((marbles ((marbles p) 0)) 0) 1] next-marble))
     next-marble
     (inc next-marble)
     scores]
    (let [marble-to-remove (nth (iterate #((marbles %) 1) p) 7)
          marble-to-remove-val (marbles marble-to-remove)]
      [(-> marbles
           (conj [])
           (assoc-in [(marble-to-remove-val 1) 0]
                     (marble-to-remove-val 0))
           (assoc-in [(marble-to-remove-val 0) 1]
                     (marble-to-remove-val 1)))
       (marble-to-remove-val 0)
       (inc next-marble)
       (update scores (mod (dec next-marble) (count scores)) + next-marble marble-to-remove)])))

(defn play-marbles [num-players last-marble]
  (apply max (loop [[marbles p next-marble scores] [[[0 0]] 0 1 (vec (repeat num-players 0))]]
               (if (= next-marble (inc last-marble)) scores
                   (recur (insert-marble marbles p next-marble scores))))))

(play-marbles 465 71940)
;; => 384475

;; Part 2
(play-marbles 465 7194000)
;; => 3187566597
