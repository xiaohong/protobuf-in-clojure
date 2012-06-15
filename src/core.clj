(ns core
  (:refer-clojure :excludes [read-string])
  (:use clojure.stacktrace))

(defn write-int
  [i]
  (loop [c [] a i ]
    (if (= 0 (bit-and a (bit-not 0x7f)))
      (conj c (byte a))
      (recur (conj c
                   (bit-or 0x80 (bit-and a 0x7F)))
             (bit-shift-right a 7)))))


(defn read-varint
  [f]
  (loop [[a & r] f t 0 i 0]
    (if (= 0 (bit-and (bit-not 0x7f) a))
      [(bit-or (bit-shift-right t (* i 7))
              a) r ]
      (recur r (bit-or (bit-shift-right t (* i 7))
                       a) (inc i)))))
(defn read-fixed64
  [data]
  (let [[a b c d e f g h & r] data]
    [(bit-or
      (bit-shift-left h 56)
      (bit-shift-left g 48)
      (bit-shift-left f 40)
      (bit-shift-left e 32)
      (bit-shift-left d 24)
      (bit-shift-left c 16)
      (bit-shift-left b 8)
      a)
     r]))

(defn read-string
  [data]
  (let [[l r] (read-varint data)]
    [(String. (byte-array (take l r)))
     (drop l r)]))

(defn read-fixed32
  [data]
  (let [[a b c d & r] data]
    [(bit-or
       (bit-shift-left d 24)
       (bit-shift-left c 24)
       (bit-shift-left b 24)
       a)
     r]))

(defn read-pf
  [bs]
  (loop [[c & f] bs n []]
    (if c
      (let [type (bit-and c 0x07)]
        (cond
          (= 0 type) (let [[d r] (read-varint f)]
                       (recur r (conj n d)))
          (= 1 type) (let [[d r] (read-fixed64 f)]
                       (recur r (conj n d)))
          (= 2 type) (let [[d r] (read-string f)]
                       (recur r (conj n d)))
          (= 5 type) (let [[d r] (read-fixed32 f)]
                       (recur r (conj n d)))
          :else n)))))

(defn to-bytes
  [t v]
  (cond 
    (= t :int) [(bit-and v 0xFF)
                (bit-and (bit-shift-right v 8) 0xFF)
                (bit-and (bit-shift-right v 16) 0xFF)
                (bit-and (bit-shift-right v 24) 0xFF)]
    (= t :string)
       (let [d (seq (.getBytes v))] 
         (cons (count d) (vec d)))
    :else []))

(defn type-tag
  [t]
  (cond
    (= t :int) 0
    (= t :string) 1
    :else 2))

(defn tag->type
  [t]
  (get {0 :int 1 :string} t))

(defn make-tag
  [i t]
  (write-varint (bit-or (bit-shift-left i 3)
          (type-tag t))))

(defn write-varint
  [v]
  (write-int v))

(defn write-val
  [t v]
  (cond 
    (= t :int) (write-varint v)
    (= t :string) (let [d (seq (.getBytes v))] 
         (concat (write-varint (count d)) (vec d)))
    :else []))

(defn encode
  [s data]
  (loop [[a & r] s i 1 t []]
    (if a
      (let [[name type] a
            tag (make-tag i type)
            v (get data name nil)]
        (if v
          (recur r (inc i) (concat t tag (write-val type v)))
          (recur r (inc i) t)))
      t)))

(defn read-tag
  [t]
  (bit-shift-right t 3))

(defn read-type
  [t]
  (bit-and t 0x7))

(defn read-val
  [t v]
  (cond
    (= t :int) (read-varint v)
    (= t :string) (let [[len r] (read-varint v)]
                    [(String. (byte-array (map #(byte %) (take len r))))
                     (drop len r)])))

(defn decode
  [s data]
  (loop [[a r] (read-varint data) o {}]
    (if a
      (let [no (read-tag a)
            type (read-type a)
            [v rr] (read-val (tag->type type) r)]
        (if (seq rr)
          (recur (read-varint rr) (assoc o (first (s (dec no))) v))
          (assoc o (first (s (dec no))) v)))
        o)))

(def spec
  [[ :id :int]
   [ :name :string]
   [:country :int]
   [:school :string]])

(encode spec {:id 1123 :name "test" :school "山西的地方是打发打发多少"})

(decode spec (encode spec {:id 1123 :name "test" :school "山西的地方是打发打发多少"}))

