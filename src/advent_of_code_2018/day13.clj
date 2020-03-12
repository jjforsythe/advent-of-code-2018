;; Part 1
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input13.txt")))

(defn parse-map []
  (mapv #(str/split (str/replace % #"[\^>v<]" {"^" "|", "v" "|", ">" "-", "<" "-"}) #"")
        input))

(def parsed-map (parse-map))

(defn map-input-coords [lines]
  (reduce concat (map-indexed
                  (fn [index line] (map-indexed #(vector [%1 index] (str %2)) line))
                  lines)))

(defn parse-carts [lines]
  (reduce #(if (contains? #{"v" "^" ">" "<"} (second %2)) (conj %1 [(first %2) (second %2) 0]) %1)
          [] lines))

(defn sort-carts [carts]
  (vec (sort-by #(vec (reverse (first %)))
                carts)))

(def initial-carts
  (sort-carts (parse-carts (map-input-coords input))))

(defn check-collision [carts cart]
  (> (count (filter #(= % (first cart)) (map first carts))) 0))

(def left-map {">" "^", "^" "<", "<" "v", "v" ">"})
(def right-map {">" "v", "v" "<", "<" "^", "^" ">"})

(defn next-cart-state [[[x y] direction turns]]
  (let [next-position (cond (= direction ">") [(inc x) y]
                            (= direction "<") [(dec x) y]
                            (= direction "^") [x (dec y)]
                            (= direction "v") [x (inc y)])]
    (apply conj [next-position]
           (let [next-track (get (parsed-map (second next-position)) (first next-position))]
             (cond (= "+" next-track)
                   [(cond (= turns 0) (get left-map direction)
                          (= turns 1) direction
                          (= turns 2) (get right-map direction))
                    (mod (inc turns) 3)]
                   (= "/" next-track)
                   [(cond (= direction "^") ">"
                          (= direction "<") "v"
                          (= direction ">") "^"
                          (= direction "v") "<")
                    turns]
                   (= "\\" next-track)
                   [(cond (= direction "^") "<"
                          (= direction "<") "^"
                          (= direction ">") "v"
                          (= direction "v") ">")
                    turns]
                   true [direction turns])))))

(defn tick []
  (loop [index 0
         carts initial-carts]
    (let [cart (carts (mod index (count carts)))
          updated-cart (next-cart-state cart)
          updated-carts (assoc carts (mod index (count carts)) updated-cart)
          collision? (check-collision carts updated-cart)]
      (if collision? updated-cart
          (recur
           (inc index)
           (if (= (dec (count updated-carts)) (mod index (count updated-carts)))
             (sort-carts updated-carts) updated-carts))))))

(tick)
;; => [[41 17] "^" 1]

;; Part 2
(defn tick-2 []
  (loop [index 0
         carts initial-carts]
    (cond (and (= 0 (mod index (count carts)))
               (>= 1 (count (filter #(not= [-1 -1] (first %)) carts)))) carts
          (= (first (carts (mod index (count carts))))  [-1 -1])
          (recur (inc index) carts)
          true (let [cart (carts (mod index (count carts)))
                     updated-cart (next-cart-state cart)
                     updated-carts (assoc carts (mod index (count carts)) updated-cart)
                     collision? (check-collision carts updated-cart)
                     filtered-carts (if collision?
                                      (mapv #(if (= (first %) (first updated-cart))
                                               (assoc % 0 [-1 -1]) %) updated-carts)
                                      updated-carts)]
                 (recur
                  (inc index)
                  (if (= (dec (count updated-carts)) (mod index (count updated-carts)))
                    (sort-carts filtered-carts) filtered-carts))))))

(last (tick-2))
;; => [[134 117] "^" 0]
