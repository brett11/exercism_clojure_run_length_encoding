(ns run-length-encoding)

(defn reduce-repeating-chrs [lst]
  "takes a lst that will consist of one or more the same chars, e.g., (\\A \\A) or (\\A), and turns it into \"2A\" or \"A\" respectively"
  (if (= (count lst) 1)
    (str (first lst))
    (str (count lst) (first lst))))

(defn run-length-encode
  "encodes a string with run-length-encoding"
  [s]
  (->>
    s
    (partition-by identity)
    (map reduce-repeating-chrs)
    (apply str)))

(defn unpack-repeating-numbers
  [num chr]
  (apply str (repeat num chr))
  )

(defn unpack [s]
  "takes string like \"3s\" or \"s\" and unpacks it to \"sss\" and \"s\" respectively"
  (if (Character/isDigit (first s))
    (let [split (split-with #(Character/isDigit %) s)
           num (Integer/parseInt (apply str (first split)))
           ltr (first (second split))]
           (unpack-repeating-numbers num ltr))
    s))

(defn run-length-decode
  "decodes a run-length-encoded string"
  [s]
  (->>
    s
    (re-seq #"\d*\D{1}")
    (map unpack)
    (apply str)))

