(ns mazes-clj.grid)

(defn grid
  "Creates a grid of `rows` by `cols` unlinked cells."
  [rows cols]
  (assert (pos? rows) "# of rows must be positive")
  (assert (pos? cols) "# of columns must be positive")

  {:rows       rows
   :cols       cols
   :links      {}})

(defn valid-cell?
  "Checks that the given cell has valid grid coordinates, i.e., both are positive."
  [cell]
  (and (pos? (:x cell))
       (pos? (:y cell))))

(defn has-cell?
  "Returns true when the cell is a valid grid cell located on the grid."
  [grid c]
  (and (valid-cell? c)
       (>= (:rows grid) (:x c))
       (>= (:cols grid) (:y c))))

(defn cell
  "Creates a cell with `x` and `y` coordinates."
  [x y]
  {:x x
   :y y})

(defn cell-diff
  "Returns the difference of `c1` and `c2`."
  [c1 c2]
  {:x (- (:x c1) (:x c2))
   :y (- (:y c1) (:y c2))})

(defn coord-sum
  "Returns the sum of the cell's x and y coordinates."
  [c]
  (+ (:x c) (:y c)))

(defn cell-neighbours?
  "Returns true when `c1` and `c2` are neighbours."
  [c1 c2]
  (->> (cell-diff c1 c2)
       coord-sum
       Math/abs
       (= 1)))

(defn neighbours?
  "Returns true when `c1` and `c2` are neighbours in the `grid`."
  [grid c1 c2]
  (and (has-cell? grid c1)
       (has-cell? grid c2)
       (cell-neighbours? c1 c2)))

(defn cell-neighbours
  "Returns the neighbours of `c`."
  [c]
  (let [x (:x c)
        y (:y c)]
    [(cell (dec x) y)
     (cell (inc x) y)
     (cell x (dec y))
     (cell x (inc y))]))

(defn neighbours
  "Returns the neighbours of `c` in the `grid`."
  [grid c]
  (filter #(has-cell? grid %) (cell-neighbours c)))

(defn link
  "Unilaterally links `c1` to `c2` in the `grid`."
  [grid c1 c2]
  (update-in grid [:links c1] conj c2))

(defn unlink
  "Unilaterally unlinks `c1` from `c2` in the `grid`."
  [grid c1 c2]
  (update-in grid [:links c1] disj c2))

(defn links
  "Returns all links of `c` in the `grid`."
  [grid c]
  (get-in grid [:links c]))

(defn linked?
  "Returns true when `c1` and `c2` are linked in the `grid`."
  [grid c1 c2]
  (boolean (get-in grid [:links c1] c2)))
