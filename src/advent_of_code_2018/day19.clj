;; Part 1
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input19.txt")))

(def instructions
  (->> (map #(str/split % #" ") (rest input))
       (mapv #(apply conj (vector (symbol (first %)))
                     (map (fn [x] (Integer/parseInt x)) (rest %))))))

(def mod-reg (Integer/parseInt (re-find #"\d+" (input 0))))

(defmacro make-fn [prefix key op]
  (let [sym (->> key name (str prefix) symbol)]
    `(defn ~sym [regs# ins#]
       (assoc regs# (ins# 3)
              (~op (regs# (ins# 1))
               (if (= ~key :r)
                 (regs# (ins# 2))
                 (ins# 2)))))))

(make-fn add :r +)
(make-fn add :i +)
(make-fn mul :i *)
(make-fn mul :r *)
(make-fn ban :r bit-and)
(make-fn ban :i bit-and)
(make-fn bor :r bit-or)
(make-fn bor :i bit-or)

(defn setr [regs ins]
  (assoc regs (ins 3)
         (regs (ins 1))))

(defn seti [regs ins]
  (assoc regs (ins 3)
         (ins 1)))

(defn gtir [regs ins]
  (assoc regs (ins 3)
         (if (> (ins 1)
                (regs (ins 2))) 1 0)))

(defn gtri [regs ins]
  (assoc regs (ins 3)
         (if (> (regs (ins 1))
                (ins 2)) 1 0)))

(defn gtrr [regs ins]
  (assoc regs (ins 3)
         (if (> (regs (ins 1))
                (regs (ins 2))) 1 0)))

(defn eqir [regs ins]
  (assoc regs (ins 3)
         (if (= (ins 1)
                (regs (ins 2))) 1 0)))

(defn eqri [regs ins]
  (assoc regs (ins 3)
         (if (= (regs (ins 1))
                (ins 2)) 1 0)))

(defn eqrr [regs ins]
  (assoc regs (ins 3)
         (if (= (regs (ins 1))
                (regs (ins 2))) 1 0)))

(defn exec [[p regs]]
  (let [new-regs ((resolve ((instructions p) 0)) (assoc regs mod-reg p) (instructions p))]
    [(inc (new-regs mod-reg)) new-regs]))

(defn take-while+
  [pred coll]
  (lazy-seq
   (when-let [[f & r] (seq coll)]
     (if (pred f)
       (cons f (take-while+ pred r))
       [f]))))

(-> (last (take-while+ #(< (first %) (count instructions)) (iterate exec [0 [0 0 0 0 0 0]])))
    (get-in [1 0]))
;; => 2821

;; Part 2
;; Use the following code to explore the program's behaviour
(take-last 100 (take-while+ #(< (first %) (count instructions)) (iterate exec [0 [0 0 0 0 0 0]])))

;; The program computes a number, stores it into register 1 and then computes the sum
;; of all of the factors of this number

(defn sum-all-factors [n]
  (reduce + (filter #(not= -1 %) (map #(if (= (mod n %) 0) % -1) (range 1 (inc n))))))

;; This also works for Part 1
(sum-all-factors 900)
;; => 2821

;; Get the number
(-> (last (take 18 (iterate exec [0 [1 0 0 0 0 0]])))
    (get-in [1 1]))
;; => 10551300

(sum-all-factors 10551300)
;; => 30529296
