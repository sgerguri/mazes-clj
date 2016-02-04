(ns mazes-clj.grid)

(defn grid
  "Creates a grid of `rows` by `cols` unlinked cells."
  [rows cols]
  (assert (pos? rows) "# of rows must be positive")
  (assert (pos? cols) "# of columns must be positive")

  {:rows       rows
   :cols       cols
   :links      {}})

(defn size
  "Returns the number of cells in the `grid`."
  [grid]
  (* (:rows grid) (:cols grid)))

(defn valid-cell?
  "Checks that the given cell has valid grid coordinates, i.e., both are positive."
  [cell]
  (and (pos? (:row cell))
       (pos? (:col cell))))

(defn has-cell?
  "Returns true when the cell is a valid grid cell located on the grid."
  [grid c]
  (and (valid-cell? c)
       (>= (:rows grid) (:row c))
       (>= (:cols grid) (:col c))))

(defn cell
  "Creates a cell located in `row` and `col`."
  [row col]
  {:row row
   :col col})

(defn random-cell
  "Returns a random cell on the grid."
  [grid]
  (let [row (inc (rand-int (:rows grid)))
        col (inc (rand-int (:cols grid)))]
    (cell row col)))

(defn cell-diff
  "Returns the difference of `c1` and `c2`."
  [c1 c2]
  (cell (- (:row c1) (:row c2)) (- (:col c1) (:col c2))))

(defn coord-sum
  "Returns the sum of the cell's row and column."
  [c]
  (+ (:row c) (:col c)))

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
  (let [row (:row c)
        col (:col c)]
    {:north (cell (inc row) col)
     :south (cell (dec row) col)
     :west  (cell row (dec col))
     :east  (cell row (inc col))}))

(defn neighbours
  "Returns the neighbours of `c` in the `grid`."
  [grid c]
  (->> (cell-neighbours c)
       (filter (fn [[_border cell]] (has-cell? grid cell)))
       (into {})))

(defn update-link
  [grid c1 c2 f bidir?]
  (if bidir?
    (-> grid
        (update-link c1 c2 f false)
        (update-link c2 c1 f false))
    (update-in grid [:links c1] #(f (set %) c2))))

(defn link
  "Links `c1` to `c2` in the `grid`.
  If `bidir?` is false, the link will be unidirectional instead."
  ([grid c1 c2]
    (link grid c1 c2 true))
  ([grid c1 c2 bidir?]
    (update-link grid c1 c2 conj bidir?)))

(defn unlink
  "Unlinks `c1` from `c2` in the `grid`.
  If `bidir?` is false, the unidirectional link from `c2` to `c1` will be preserved."
  ([grid c1 c2]
    (unlink grid c1 c2 true))
  ([grid c1 c2 bidir?]
    (update-link grid c1 c2 disj bidir?)))

(defn links
  "Returns all links of `c` in the `grid`."
  [grid c]
  (get-in grid [:links c]))

(defn linked?
  "Returns true when `c1` and `c2` are linked in the `grid`."
  [grid c1 c2]
  (boolean (get-in grid [:links c1] c2)))

(defn row-seq
  "Returns a seq on the rows in the grid."
  [grid]
  (for [row (range 1 (inc (:rows grid)))]
    (for [col (range 1 (inc (:cols grid)))]
      (cell col row))))

(defn cell-seq
  "Returns a seq on the cells in the grid, ordered by row and column."
  [grid]
  (for [row (range 1 (inc (:rows grid)))
        col (range 1 (inc (:cols grid)))]
    (cell col row)))
