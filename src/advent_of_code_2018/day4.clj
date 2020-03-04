;; Part 1
(require '[clojure.string :as str])
(import (java.time.format DateTimeFormatter)
        (java.time LocalDateTime))

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input4.txt")))

(defn sort-by-date [a b]
  (.compareTo (LocalDateTime/parse a (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))
              (LocalDateTime/parse b (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))))

(def sorted-input (vec (sort-by #(subs % 1 17) sort-by-date input)))
;; (spit "./src/advent_of_code_2018/input4-sorted.txt" (print-str (mapv #(str % "\n") sorted-input)))

(def guard-sleep-map
  (loop [index 0
         guards {}
         current-guard ""]
    (cond (>= index (count sorted-input)) guards
          (some? (re-find #"#\d+" (get sorted-input index)))
          (recur (inc index)
                 (assoc guards (re-find #"#\d+" (get sorted-input index))
                        (if (> (count (get guards
                                           (re-find #"#\d+" (get sorted-input index)))) 0)
                          (get guards (re-find #"#\d+" (get sorted-input index)))
                          []))
                 (re-find #"#\d+" (get sorted-input index)))
          (some? (re-find #"falls asleep" (get sorted-input index)))
          (recur (inc (inc index))
                 (assoc guards current-guard
                        (vec (concat
                              (get guards current-guard)
                              (range (Integer/parseInt
                                      (subs (get sorted-input index) 15 17))
                                     (Integer/parseInt
                                      (subs (get sorted-input (inc index)) 15 17)))))
                        )
                 current-guard)
          )))

(* (get (apply max-key val
               (frequencies (val
                             (apply max-key #(count (val %))
                                    guard-sleep-map)))) 0)
   (Integer/parseInt (subs (key
                            (apply max-key #(count (val %))
                                   guard-sleep-map)) 1)))
;; => 98680

;; Part Two
(reduce * ((juxt (comp #(Integer/parseInt %) #(subs % 1) first) (comp first second)) 
           (apply max-key #(if (> (count (val %)) 0)  (second (val %)) -1)
                  (reduce-kv #(assoc %1 %2
                                     (if (> (count %3) 0)
                                       (apply max-key val (frequencies %3)) []))
                             {} guard-sleep-map))))
;; => 9763
