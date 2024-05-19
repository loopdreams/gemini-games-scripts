(require '[space-age.db :as db])
(require '[space-age.responses :as r])
(require '[space-age.user-registration :as reg])
(require '[clojure.string :as str])

(def root "/src/app/chess")
(def break "\n\n")

;; Pieces

(def blank-marker (str (char 183)))

(def white-K (str (char 9812)))
(def white-Q (str (char 9813)))
(def white-R (str (char 9814)))
(def white-B (str (char 9815)))
(def white-Kn (str (char 9816)))
(def white-P (str (char 9817)))

(def black-K (str (char 9818)))
(def black-Q (str (char 9819)))
(def black-R (str (char 9820)))
(def black-B (str (char 9821)))
(def black-Kn (str (char 9822)))
(def black-P (str (char 9823)))

(def default-board
  [[black-R black-Kn black-B black-Q black-K black-B black-Kn black-R]
   (into [] (repeat 8 black-P))
   (into [] (repeat 8 blank-marker))
   (into [] (repeat 8 blank-marker))
   (into [] (repeat 8 blank-marker))
   (into [] (repeat 8 blank-marker))
   (into [] (repeat 8 white-P))
   [white-R white-Kn white-B white-Q white-K white-B white-Kn white-R]])

(def white-K-alt "k")
(def white-Q-alt "q")
(def white-R-alt "r")
(def white-B-alt "b")
(def white-Kn-alt "n")
(def white-P-alt "p")

(def black-K-alt "K")
(def black-Q-alt "Q")
(def black-R-alt "R")
(def black-B-alt "B")
(def black-Kn-alt "N")
(def black-P-alt "P")

(def white-pieces #{white-K-alt white-Q-alt white-R-alt white-B-alt white-Kn-alt white-P-alt})
(def black-pieces #{black-K-alt black-Q-alt black-R-alt black-B-alt black-Kn-alt black-P-alt})

(def default-board-alt
  [[black-R-alt black-Kn-alt black-B-alt black-Q-alt black-K-alt black-B-alt black-Kn-alt black-R-alt]
   (into [] (repeat 8 black-P-alt))
   (into [] (repeat 8 blank-marker))
   (into [] (repeat 8 blank-marker))
   (into [] (repeat 8 blank-marker))
   (into [] (repeat 8 blank-marker))
   (into [] (repeat 8 white-P-alt))
   [white-R-alt white-Kn-alt white-B-alt white-Q-alt white-K-alt white-B-alt white-Kn-alt white-R-alt]])

(def blank-board (into [] (repeat 8 (into [] (repeat 8 blank-marker)))))


#_(def test-board
    (update-board default-board-alt {:from [1 6]
                                     :to [1 2]
                                     :piece "p"
                                     :capture false}))


;; Make board

(defn draw-board [board]
  (let [col-ref ["a" "b" "c" "d" "e" "f" "g" "h"]
        row-ref ["8" "7" "6" "5" "4" "3" "2" "1"]
        h-rows (str "   " (str/join " " col-ref) " ")
        h-line "---------------------"]
    (str
     h-rows
     "\n"
     h-line
     "\n"
     (str/join "\n"
               (for [i (range (count board))
                     :let [border (nth row-ref i)
                           board (str/join " " (nth board i))]]
                 (str border " |" board "| " border)))
     "\n"
     h-line
     "\n"
     h-rows)))


;; Game logic
;; Game logic map:
;; {board from to capture turn}

(defn board-lookup [board [x y]]
  (nth (nth board y) x))

(defn occupied? [board [x y]]
  (not= blank-marker (board-lookup board [x y])))

(defn update-square [board [x y] & piece]
  (->> (assoc (nth board y) x (if piece (first piece) blank-marker))
       (assoc board y)))

(defn update-board [board {:keys [from to capture? piece]}]
  (if (and (occupied? board to) (not capture?))
    "TODO Prompt for Correct Input"
    (-> (update-square board from)
        (update-square to piece))))

;; Valid moves for:
;; - DONE Pawn
;; - DONE Rook
;; - DONE Bishop
;; - DONE Knight
;; - WAIT King
;; - DONE Queen


(defn player-pieces [turn]
  (if (= turn :white) white-pieces black-pieces))

(defn opponent-pieces [turn]
  (if (= turn :white) black-pieces white-pieces))

(defn valid-to [lookup to turn]
  (or (= blank-marker (lookup to))
      ((opponent-pieces turn) (lookup to))))

(defn diagonal? [[x1 y1] [x2 y2]]
  (= (abs (- x2 x1))
     (abs (- y2 y1))))

;; Pawn
(defn valid-move-P [{:keys [board from to turn]}]
  (let [player-pieces   (player-pieces turn)
        opponent-pieces (opponent-pieces turn)
        [x1 y1]         from
        lookup          (partial board-lookup board)
        starting-points (into #{} (for [i (range 8)]
                                    [i (if (= turn :white) 6 1)]))
        direction       (if (= turn :white) - +)]
    (when (player-pieces (lookup from))
      (cond
        (= blank-marker (lookup to)) (or (= [x1 (direction y1 1)] to)
                                         (and (starting-points from)
                                              (= [x1 (direction y1 2)] to)))
                                              
        (opponent-pieces (lookup to)) (or (= [(dec x1) (direction y1 1)] to)
                                          (= [(inc x1) (direction y1 1)] to))
        :else false))))

;; Knight
(defn valid-move-N [{:keys [board from to turn]}]
  (let [player-pieces   (player-pieces turn)
        [x1 y1]         from
        lookup          (partial board-lookup board)]
    (when (and (player-pieces (lookup from))
               (not (player-pieces (lookup to)))
               (lookup to))
      (or
       (= to [(+ x1 2) (- y1 1)])
       (= to [(- x1 2) (- y1 1)])
       (= to [(+ x1 2) (+ y1 1)])
       (= to [(- x1 2) (+ y1 1)])
       (= to [(+ x1 1) (+ y1 2)])
       (= to [(- x1 1) (+ y1 2)])
       (= to [(+ x1 1) (- y1 2)])
       (= to [(- x1 1) (- y1 2)])))))

;; Rook
(defn valid-move-R [{:keys [board from to turn]}]
  (let [[x1 y1] from
        [x2 y2] to
        lookup  (partial board-lookup board)]
    (when (or (= x1 x2) (= y1 y2))
      (let [between-squares (-> (for [i (apply range (sort (if (= x1 x2) [y1 y2] [x1 x2])))]
                                  (if (= x1 x2) [x1 i] [i y1]))
                                rest)]
        (when (every? #(= blank-marker %) (map lookup between-squares))
          (valid-to lookup to turn))))))


(defn valid-move-B [{:keys [board from to turn]}]
  (when (diagonal? from to)
    (let [[x1 y1]    from
          [x2 y2]    to
          lookup     (partial board-lookup board)
          btw-xs     (rest (apply range (sort [x1 x2])))
          btw-ys     (rest (apply range (sort [y1 y2])))
          btw-points (partition-all 2 (interleave btw-xs btw-ys))]
      (when (every? #(= blank-marker %) (map lookup btw-points))
        (valid-to lookup to turn)))))

;; Queen
(defn valid-move-Q [m]
  (or (valid-move-B m)
      (valid-move-R m)))

;; King
;; TODO add 'check' restriction
(defn valid-move-K [{:keys [board from to turn]}]
  (let [[x1 y1] from
        [x2 y2] to]
    (when (or (and (= 1 (abs (- x2 x1))) (= y2 y1))
              (and (= 1 (abs (- y2 y1))) (= x2 x1))
              (and (= 1 (abs (- x2 x1))) (= 1 (abs (- y2 y1)))))
      (valid-to (partial board-lookup board) to turn))))



(comment
  (valid-move-R {:board (-> (update-square blank-board [0 0] "R")
                            (update-square [0 1] "p"))
                 :from [0 0] :to [0 1] :turn :black})
  (valid-move-N {:board default-board-alt :from [2 0] :to [3 2] :turn :black})
  (valid-move-R {:board default-board-alt :from [2 2] :to [6 3] :turn :black})
  (valid-move-P {:board default-board-alt :from [2 1] :to [1 2] :turn :black}))
  


;; TODO Check recognition
;; TODO Checkmate recognition
;; TODO Castling
;; TODO New pieces


(comment
  (draw-board
   (update-board default-board-alt {:from [6 7]
                                    :to [5 5]
                                    :piece "n"
                                    :capture false})))

;; Input Handling



;; Turn logic




;; Starting a game


;; Playing a game


;; Main Page

(defn main-page [req]
  (->> (str "# Chess"
            break
            "```\n"
            (draw-board default-board-alt)
            break
            (draw-board default-board)
            "\n```\n")
       (r/success-response r/gemtext)))


;; Main/routes


(defn main [req]
  (main-page req))
