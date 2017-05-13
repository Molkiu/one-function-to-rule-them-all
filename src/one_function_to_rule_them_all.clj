(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if(empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest(reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn[n _](inc n)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce conj () a-seq))

(defn min-max-element [a-seq]
  (vector (reduce min a-seq)
          (reduce max a-seq)))

(defn insert [sorted-seq n]
  (concat (filter #(< % n) sorted-seq) [n] (filter #(> % n) sorted-seq)))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (letfn [(toggle [a-set elem]
            (if(contains? a-set elem)
              (disj a-set elem)
              (conj a-set elem)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params 
  ([] 0)
  ([x] 1)
  ([x & more] (+ (count-params x) (my-count more))))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce my-* (my-* x y) more)))
(defn pred-and
  ([] (fn [x] (== x x)))
  ([pred] (fn [x] (pred x)))
  ([pred1 pred2] (fn [x] (and (pred1 x) (pred2 x))))
  ([pred1 pred2 & more] (reduce pred-and (pred-and pred1 pred2) more)))

(defn my-map
  ([f coll] (seq (reduce #(conj %1 (f %2)) [] coll)))
  ([f coll & colls]
    (let [colls (cons coll colls)]
      (my-map (partial apply f)
              (partition (count colls) 
                         (apply interleave colls))))))
