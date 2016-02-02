(ns mazes-clj.grid)

(defn cell
  ([x y]
    (cell x y #{}))
  ([x y links]
    {:x     x
     :y     y
     :links links}))

(defn update-links
  [cell1 cell2 f]
  (->> (dissoc cell2 :links)
       (update-in cell1 [:links] f)))

(defn link
  [cell1 cell2]
  (update-links cell1 cell2 conj))

(defn unlink
  [cell1 cell2]
  (update-links cell1 cell2 disj))

(defn links
  [cell]
  (:links cell))

(defn linked?
  [cell1 cell2]
  (->> (dissoc cell2 :links)
       (get (:links cell1))
       boolean))

(defn cell-diff
  [cell1 cell2]
  {:x (- (:x cell1) (:x cell2))
   :y (- (:y cell1) (:y cell2))})

(defn coord-sum
  [cell]
  (+ (:x cell) (:y cell)))

(defn neighbours?
  [cell1 cell2]
  (and (linked? cell1 cell2)
       (->> (cell-diff cell1 cell2)
            coord-sum
            Math/abs
            (= 1))))

(defn neighbours
  [cell]
  (filter #(neighbours? cell %) (links cell)))
