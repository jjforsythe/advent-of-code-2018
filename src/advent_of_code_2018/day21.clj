;; Part 1
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input21.txt")))

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

(get-in (last (take-while+ #(not= 28 (% 0)) (iterate exec [0 [0 0 0 0 0 0]]))) [1 3])
;; => 13270004

;; Part Two
;; From state when executing instruction 28 until we are back
;; at instruction 28
(defn ins-8-to-12 [[reg-3 reg-5]]
  (-> (bit-and reg-5 255)
      (+ reg-3)
      (bit-and 16777215)
      (* 65899)
      (bit-and 16777215)))

(defn ins-6-to-12 [reg-3]
  (let [reg-5 (bit-or reg-3 65536)]
    [(ins-8-to-12 [15028787 reg-5])
     reg-5]))

(defn main-loop [[reg-3 reg-5]]
  (let [new-reg-5 (#(dec (int (Math/ceil (/ (inc reg-5) 256)))))
        new-reg-3 (ins-8-to-12 [reg-3 new-reg-5])]
    (if (< new-reg-5 256) new-reg-3 (recur [new-reg-3 new-reg-5]))))

;; Use this to explore the execution order of instructions
(take 2000 (iterate exec [0 [0 0 0 0 0 0]]))

;; This filters the program's steps for the state when executing instruction 28
(filter #(= 28 (% 0)) (take 6000000 (iterate exec [0 [0 0 0 0 0 0]])))

;; Eventually the program hits a loop
(filter #(> (val %) 1) (frequencies (take 13006 (iterate (comp main-loop ins-6-to-12) 13270004))))
;; => ([8499279 2] [5049921 2])

;; Answer
(last (take (- 13006 2) (iterate (comp main-loop ins-6-to-12) 13270004)))
;; => 12879142
