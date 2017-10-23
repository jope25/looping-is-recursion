(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn  [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn  [last a-seq]
                 (if (empty? a-seq)
                   last
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or
      (empty? seq1)
      (empty? seq2)
      (not (= (first seq1) (first seq2)))) false 
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         seq a-seq]
    (cond
      (empty? seq) nil
      (pred (first seq)) index
      :else (recur (inc index) (rest seq)))))

(defn avg [a-seq]
  (loop [total 0
         count 0
         seq a-seq]
    (if (empty? seq)
      (/ total count)
      (recur (+ total (first seq)) (inc count) (rest seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                (if (contains? a-set elem)
  	              (disj a-set elem)
  	              (conj a-set elem)))]
    (loop [set #{}
           seq a-seq]
      (if (empty? seq)
       set
       (recur (toggle set (first seq)) (rest seq))))))

(defn fast-fibo [n]
  (loop [counter 2
         n-1 1
         n-2 0]
    (cond 
      (< n 2) n
      (== counter n) (+ n-1 n-2)
      :else (recur (inc counter) (+ n-1 n-2) n-1))))

(defn cut-at-repetition [a-seq]
  (loop [seq a-seq
         vec []]
    (cond
      (empty? seq) vec
      (contains? (set vec) (first seq)) vec
      :else (recur (rest seq) (conj vec (first seq))))))
