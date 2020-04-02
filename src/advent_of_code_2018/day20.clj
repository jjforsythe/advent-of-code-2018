;; Part 1
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input20.txt")))

(def parsed-input
  (subs (input 0) 1 (dec (count (input 0)))))

(defn west [[x y]]
  [[(- x 2) y]
   [(dec x) y]])

(defn east [[x y]]
  [[(+ x 2) y]
   [(inc x) y]])

(defn north [[x y]]
  [[x (+ y 2)]
   [x (inc y)]])

(defn south [[x y]]
  [[x (- y 2)]
   [x (dec y)]])

(defn assign-values [[room door]]
  [room "room" door "door"])

(def test-routes-1 "WNE")
(def test-routes-2 "ENWWW(NEEE|SSE(EE|N))")
(def test-routes-3 "ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN")
(def test-routes-4 "ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))")
(def test-routes-5 "WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))")

(def facility-map
  (let [routes parsed-input]
    (loop [index 0
           pos [0 0]
           parents []
           result {}]
      (cond (= index (count routes)) result
            (= (get routes index) \() (recur (+ index 1) pos (conj parents pos) result)
            (= (get routes index) \)) (recur (+ index 1) (last parents) (pop parents) result)
            (= (get routes index) \|) (recur (+ index 1) (last parents) parents result)
            (= (get routes index) \N) (recur (+ index 1) ((north pos) 0) parents
                                             (apply assoc result (assign-values (north pos))))
            (= (get routes index) \E) (recur (+ index 1) ((east pos) 0) parents
                                             (apply assoc result (assign-values (east pos))))
            (= (get routes index) \S) (recur (+ index 1) ((south pos) 0) parents
                                             (apply assoc result (assign-values (south pos))))
            (= (get routes index) \W) (recur (+ index 1) ((west pos) 0) parents
                                             (apply assoc result (assign-values (west pos))))
            true (recur (inc index) pos parents result)))))

(defn room-neighbours [doors [x y]]
  (-> []
      (#(if (doors [(inc x) y]) (conj % [(+ x 2) y]) %))
      (#(if (doors [(dec x) y]) (conj % [(- x 2) y]) %))
      (#(if (doors [x (inc y)]) (conj % [x (+ y 2)]) %))
      (#(if (doors [x (dec y)]) (conj % [x (- y 2)]) %))))

(defn visit-rooms [[doors rooms next-rooms]]
  (apply conj [doors]
         (reduce #(vector (dissoc (%1 0) %2)
                          (if ((%1 0) %2)
                            (apply conj (%1 1) (room-neighbours doors %2))
                            (%1 1)))
                 [rooms []] next-rooms)))

;; Visit all rooms
(dec (count
      (take-while
       #(> (count (% 1)) 0)
       (iterate visit-rooms
                [(into {} (filter #(= (val %) "door") facility-map))
                 (into {} (filter #(= (val %) "room") (assoc facility-map [0 0] "room")))
                 [[0 0]]
                 ]))))
;; => 4308

;; Part 2
;; Count remaining rooms after we have visited all rooms within 1000 steps
(count ((last
         (take (inc 1000)
               (iterate visit-rooms
                        [(into {} (filter #(= (val %) "door") facility-map))
                         (into {} (filter #(= (val %) "room") (assoc facility-map [0 0] "room")))
                         [[0 0]]
                         ]))) 1))
;; => 8528
