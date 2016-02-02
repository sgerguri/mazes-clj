(ns mazes-clj.grid)

(defn cell
  ([x y]
    (cell x y #{}))
  ([x y links]
    {:x     x
     :y     y
     :links links}))

(defn update-links
  [c1 c2 f]
  (->> (dissoc c2 :links)
       (update-in c1 [:links] f)))

(defn link
  [c1 c2]
  (update-links c1 c2 conj))

(defn unlink
  [c1 c2]
  (update-links c1 c2 disj))

(defn links
  [c]
  (:links c))

(defn linked?
  [c1 c2]
  (->> (dissoc c2 :links)
       (get (:links c1))
       boolean))

(defn cell-diff
  [c1 c2]
  {:x (- (:x c1) (:x c2))
   :y (- (:y c1) (:y c2))})

(defn coord-sum
  [c]
  (+ (:x c) (:y c)))

(defn neighbours?
  [c1 c2]
  (and (linked? c1 c2)
       (->> (cell-diff c1 c2)
            coord-sum
            Math/abs
            (= 1))))

(defn neighbours
  [c]
  (filter #(neighbours? c %) (links c)))
