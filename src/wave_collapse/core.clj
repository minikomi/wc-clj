(ns wave-collapse.core

  (:require

   [io.aviso.ansi :as ansi]))

(def input-matrix-1
  [

    ["L" "L" "L" "L"]
    ["L" "L" "L" "L"]
    ["L" "L" "L" "L"]
    ["L" "C" "C" "L"]
    ["C" "S" "S" "C"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"]
    ["S" "S" "S" "S"] ])

(def directions
  [[:up   [-1 0]]
   [:down  [1  0]]
   [:left  [0 -1]]
   [:right [0  1]]])

(def directions-map
  (into {} directions))

(defn modify [[y x] dir]
  (let [[ymod xmod] (get directions-map dir)]
    [(+ y ymod) (+ x xmod)]))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn shanon-entropy [probabilities]
  (- (reduce #(let [p (second %2)]
                (+ % (* p (log2 p))))
             0
             probabilities)))

(defn parse-input [input]
  (let [weights (volatile! {})
        rules (volatile! #{})
        h (count input)
        w (count (first input))]
    (doseq [y (range h)
            x (range w)
            :let [c (get-in input [y x])]]
      (do
        (vswap! weights update c (fnil inc 0))
        (doseq [[dir [ymod xmod]] directions
                :let [co-ord [(+ y ymod) (+ x xmod)]
                      c' (get-in input co-ord)]
                :when c']
          (vswap! rules conj [c c' dir]))))
    {:rules @rules
     :tiles (set (map first @weights))
     :weights @weights}))

(defn co-ords-range [matrix & not-finished?]
  (for [y (range (count matrix))
        x (range (count (first matrix)))
        :let [tiles (get matrix [y x])]
        :when (or (not not-finished?) (set? tiles))]
    [y x]))

(defn lowest-entropy [{:keys [matrix uncollapsed weights ent-wiggle] :as state}]
  (->> (for [co-ord uncollapsed
             :let [tiles (get matrix co-ord)
                   selected-weights (select-keys weights tiles)
                   total-weights (apply + (map second selected-weights))
                   probabilities (into {}
                                       (map (fn [[c w]] [c (/ w total-weights)]))
                                       weights)
                   entropy (+ (rand 0.002) (shanon-entropy probabilities))]]
         [co-ord entropy])
       (sort-by second)
       ffirst))

(defn create-starting-state [input w h]
  (let [parsed (parse-input input)
        co-ords (for [y (range h)
                      x (range w)]
                  [y x])]
    (assoc parsed
           :tries 0
           :matrix (reduce
                    #(assoc % %2 (:tiles parsed))
                    {}
                    co-ords)
           :uncollapsed (set co-ords))))

(comment
  (parse-matrix input-matrix-1)
  (shanon-entropy (:weights (parse-matrix input-matrix-1)))
  (create-starting-state input-matrix1))

(defn weighted-rand-choice [m]
  (let [w (reductions #(+ % %2) (vals m))
        r (rand-int (last w))]
    (nth (keys m) (count (take-while #(<= % r) w)))))

(defn weighted-rand-choice
  "Return a random element from the weighted collection.
  A weighted collection can be any seq of [choice, weight] elements.  The
  weights can be arbitrary numbers -- they do not need to add up to anything
  specific.
  Examples:
  (rand-nth-weighted [[:a 0.50] [:b 0.20] [:c 0.30]])
  (rand-nth-weighted {:a 10 :b 200})
  "
  [coll]
  (let [total (reduce + (map second coll))]
    (loop [i (rand total)
           [[choice weight] & remaining] (seq coll)]
      (if (>= weight i)
        choice
        (recur (- i weight) remaining)))))

(defn matrix-print [matrix w h]
  (println "------- MATRIX")
  (doseq [y (range h)]
    (do (doseq [x (range w)]
          (print (case (first (get matrix [y x]))
                   "C" (ansi/yellow "C")
                   "S" (ansi/blue "S")
                   "L" (ansi/green "L")

                   )))
        (print "\n")
        (flush))))

(defn matrix-print-err [matrix w h]
  (println "------- MATRIX")
  (print "    ")
  (doseq [x (range w)]
    (print (format "%5d" x)))
  (print "\n")
  (doseq [y (range h)]
    (print (format "%5d" y))
    (do (doseq [x (range w)]
          (print (format "%5s" (apply str (get matrix [y x])))))
        (print "\n")
        (flush))))

(defn try-propagate [rules m u co-ord dir]
  (let [tiles (get m co-ord)
        n-co-ord (modify co-ord dir)
        n-tiles (get m n-co-ord)]
    (if (or (nil? n-tiles)
            (= 1 (count n-tiles))) false
        (do
          (let [r (for [nt n-tiles t tiles] [t nt dir])
                ok (filter rules r)
                n-tiles' (set (map second ok))]
            (cond
              (= n-tiles n-tiles') false
              (empty? n-tiles') (do
                                  (println "")
                                  (matrix-print-err m 40 10) ()
                                  (println co-ord n-co-ord dir "<<<< IMPOSSSIBLE")
                                  :impossible)
              :else (let [m' (assoc m n-co-ord n-tiles')
                          u' (if (= 1 (count n-tiles'))
                               (disj u n-co-ord)
                               u)]
                      [m' u' n-co-ord])))))))

(defn propagate-step [{:keys [matrix uncollapsed rules w h] :as state} co-ord selection]
  (loop [stack [co-ord]
         m (assoc matrix co-ord #{selection})
         u (disj uncollapsed co-ord)]
 ;;   (matrix-print-err m 40 10)
    (if (empty? stack) (assoc state
                              :uncollapsed u
                              :matrix m)
        (let [head (peek stack)]
          (let [stepped (some #(try-propagate rules m u head %)
                              (map first directions))]
            (cond (= stepped :impossible)
                  false
                  stepped
                  (let [[m' u' n-co-ord] stepped]
                    (recur (conj stack n-co-ord) m' u'))
                  :else
                  (recur (pop stack) m u)))))))

(defn step [{:keys [uncollapsed matrix weights rules] :as state}]
  ;;(println "----------- STEP ---------------")
  (let [co-ord (lowest-entropy state)
        tiles (get matrix co-ord)
        selection (weighted-rand-choice (select-keys weights tiles))]
    (propagate-step state co-ord selection)))

(comment

  (frequencies (take 10000 (repeatedly #(weighted-rand-choice {:L 8, :C 4, :P 2, :S 42}))))
  (let [starting (assoc (create-starting-state input-matrix-1 40 10)
                        :w 40
                        :h 10)]
    (loop [state starting]
      (cond (empty? (:uncollapsed state)) (do
                                            (println "WE DID IT")
                                            (matrix-print (:matrix state) 40 10))
            :else
            (if-let [stepped (step state)]
              (recur stepped)
              (if
               (< 5 (:tries state)) (do
                                        (println "OUT OF TRIES"))
               (do #_(matrix-print-err (:matrix state) 40 10)

                   (recur

                    (assoc starting :tries (inc (:tries state))))))))))

  )
