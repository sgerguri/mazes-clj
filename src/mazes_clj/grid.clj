(ns mazes-clj.grid)

(defn cell
  [x y]
  {:x     x
   :y     y
   :links #{}})

(defn update-neighbours
  [cell1 cell2 f]
  (->> (dissoc cell2 :links)
       (update-in cell1 [:links] f)))

(defn link
  [cell1 cell2]
  (update-neighbours cell1 cell2 conj))

(defn unlink
  [cell1 cell2]
  (update-neighbours cell1 cell2 disj))

(defn links
  [cell]
  (:links cell))

(defn linked?
  [cell1 cell2]
  (->> (dissoc cell2 :links)
       (get (:links cell1))
       boolean))
