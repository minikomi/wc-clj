(ns wave-collapse.core)

(def input-matrix-1
  [["L" "L" "L" "L"]
   ["L" "L" "L" "L"]
   ["L" "L" "L" "L"]
   ["L" "C" "C" "L"]
   ["C" "S" "S" "C"]
   ["S" "S" "S" "S"]
   ["S" "S" "S" "S"]])

(def directions
  {:up [0 -1]
   :down [0 1]
   :left [0 -1]
   :right [0 1]})

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn shanon-entropy [probabilities]
  (- (reduce #(let [p (second %2)]
                (+ %
                   (* p (log2 p))))
             0
             probabilities)))

(defn parse-matrix [matrix]
  (let [weights (volatile! {})
        rules (volatile! #{})]
    (doseq [y (range (count matrix))
            x (range (count (first matrix)))
            :let [c (get-in matrix [y x])]]
      (do
        (vswap! weights update c (fnil inc 0))
        (doseq [[dir [xmod ymod]] directions
                :let [co-ord [(+ y ymod) (+ x xmod)]
                      c' (get-in matrix co-ord)]
                :when c']
          (vswap! rules conj [c c' dir]))))
    (let [total-weights (apply + (map second @weights))
          weights-adjusted (into {}
                        (map (fn [[c w]]
                               [c (/ w total-weights)])
                             @weights))]
     {:rules @rules
      :tiles (set (map first weights-adjusted))
      :weights weights-adjusted})))



(defn co-ords-range [matrix & not-finished?]
  (for [y (range (count matrix))
        x (range (count (first matrix)))
        :let [tiles (get-in matrix [y x])]
        :when (or (not not-finished?) (set? tiles))]
    [y x]))

(co-ords-range input-matrix-1)

(defn lowest-entropy [{:keys [matrix weights] :as state}]
  (->> (for [co-ords (co-ords-range matrix true)
             :let [tiles (get-in matrix co-ords)
                   entropy (shanon-entropy (select-keys weights tiles))]]
         [co-ords entropy])
       (sort-by second)
       first))

(defn create-starting-state [input]
  (let [parsed (parse-matrix input)]
    (assoc parsed
           :tries 0
           :matrix (reduce
                    #(assoc % %2 (:tiles parsed))
                    {}
                    (co-ords-range input))
           )))


(comment
  (parse-matrix input-matrix-1)
  (shanon-entropy (:weights (parse-matrix input-matrix-1)))
  (create-starting-state input-matrix1)
  )
