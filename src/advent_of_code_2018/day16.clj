;; Part 1
(require '[clojure.string :as str])

(def input (str/split-lines (slurp "./src/advent_of_code_2018/input16-samples.txt")))

(defn parse-input [input]
  (->> input
       (partition 4 4 "")
       (map #(vector
              (read-string (re-find #"(?<=Before: ).*" (first %)))
              (read-string (str "[" (second %) "]"))
              (read-string (re-find #"(?<=After:  ).*" (nth % 2)))))))

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

(defn like-opcodes [before ins after]
  (count (filter #(= % after)
                 ((juxt addr addi mulr muli banr bani borr bori
                        setr seti gtir gtri gtrr eqir eqri eqrr)
                  before ins))))

(count (filter #(>= % 3) (map #(apply like-opcodes %) (parse-input input))))
;; => 677

;; Part 2
(def input-program (str/split-lines (slurp "./src/advent_of_code_2018/input16-program.txt")))

;; Remove opcodes from this variable as the associated
;; numbers are found
(def opcode-functions [addr addi mulr muli banr bani borr bori
                       setr seti gtir gtri gtrr eqir eqri eqrr])

(defn like-opcodes-2 [before ins after]
  (count (filter #(= % after)
                 ((apply juxt opcode-functions)
                  before ins))))

(frequencies
 (map #(vector (first (second %))
               (map (fn [x] (subs (str (type x)) 31))
                    (filter
                     (fn [f] (= (f (first %) (second %)) (last %)))
                     opcode-functions)))
      (filter #(= 1 (apply like-opcodes-2 %))
              ;; insert found opcode numbers in set below
              (filter #(not (contains? #{} (first (second %)))) (parse-input input))
              )))

(def opcodes {
              0 banr
              1 eqrr
              2 setr
              3 eqir
              4 bori
              5 muli
              6 bani
              7 borr
              8 gtir
              9 gtrr
              10 addi
              11 gtri
              12 eqri
              13 addr
              14 mulr
              15 seti
              })

(first (reduce #((opcodes (first %2)) %1 %2) [0 0 0 0]
               (map #(read-string (str "[" % "]")) input-program)))
;; => 540
