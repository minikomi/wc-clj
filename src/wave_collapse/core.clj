(ns wave-collapse.core)


(def input-matrix1
  [["L" "L" "L" "L"]
   ["L" "L" "L" "L"]
   ["L" "L" "L" "L"]
   ["L" "C" "C" "L"]
   ["C" "S" "S" "C"]
   ["S" "S" "S" "S"]
   ["S" "S" "S" "S"]]
  )

(def directions

   {:up [0 -1]
    :down [0 1]
    :left [0 -1]
    :right [0 1]}

  )

(defn parse-matrix [matrix]
  (let [weights (atom {})
        rules (atom [])]
    (doseq [y (range (count matrix))
            x (range (count (first matrix)))
            :let [c (get-in matrix [y x])]]
      (do
        (println c)
        (swap! weights update c (fnil inc 0))
        (doseq [[dir [xmod ymod]] directions
                :let [co-ord [(+ y ymod) (+ x xmod)]
                      c' (get-in matrix co-ord)]
                :when c']
          (swap! rules conj [c c' dir]))))
    {:weights @weights
     :rules @rules}))


(parse-matrix input-matrix1)
