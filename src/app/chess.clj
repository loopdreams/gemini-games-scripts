(require '[space-age.db :as db])
(require '[space-age.responses :as r])
(require '[space-age.user-registration :as reg])
(require '[clojure.string :as str])

(def root "/src/app/chess")
(def break "\n\n")

;;;; Pieces
(def blank-marker (str (char 183)))

(def white-K "k")
(def white-Q "q")
(def white-R "r")
(def white-B "b")
(def white-N "n")
(def white-P "p")

(def black-K "K")
(def black-Q "Q")
(def black-R "R")
(def black-B "B")
(def black-N "N")
(def black-P "P")

(def white-pieces #{white-K white-Q white-R white-B white-N white-P})
(def black-pieces #{black-K black-Q black-R black-B black-N black-P})

(def default-board
  [[black-R black-N black-B black-Q black-K black-B black-N black-R]
   (into [] (repeat 8 black-P))
   (into [] (repeat 8 blank-marker))
   (into [] (repeat 8 blank-marker))
   (into [] (repeat 8 blank-marker))
   (into [] (repeat 8 blank-marker))
   (into [] (repeat 8 white-P))
   [white-R white-N white-B white-Q white-K white-B white-N white-R]])

(def blank-board (into [] (repeat 8 (into [] (repeat 8 blank-marker)))))

(defn type-keyword-lookup [kw turn]
  (let [piece (case kw
                :king "K"
                :queen "Q"
                :rook "R"
                :bishop "B"
                :knight "N"
                :pawn "P")]
       (if (= turn :white)
         (str/lower-case piece)
         piece)))

(defn lookup-piece [letter]
  (case (str/lower-case letter)
    "k" :king
    "q" :queen
    "b" :bishop
    "r" :rook
    "n" :knight
    "p" :pawn))

;;;; Make board

(defn draw-board [board & rotate?]
  (let [col-ref ["a" "b" "c" "d" "e" "f" "g" "h"]
        col-ref (if (seq rotate?) (reverse col-ref) col-ref)
        row-ref ["8" "7" "6" "5" "4" "3" "2" "1"]
        row-ref (if (seq rotate?) (reverse row-ref) row-ref)
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

(defn rotate-board [board]
  (let [rotate-90 (fn [b]
                    (->> b
                         (apply mapv vector)
                         (mapv reverse)))]
    (-> board
        rotate-90
        rotate-90)))

;;;; Game logic
;; Game data ('move') is handled via a clojure map, with the following available keys:
;; {:board board-state
;; :from coords-from (e.g., [0 0])
;; :to coords-to (e.g., [0 1])
;; :turn e.g., :white
;; :piece piece-name e.g., :king
;; :piece-str piece-string e.g., "k"
;; :diambiguation-needed? boolean - used both for prompting the user to disambiguated and for notation purposes
;; :capture boolean - used for notation purposes
;; :check boolean - used for UI and notation purposes
;; :checkmate either :checkmate or :not-checkmate, same purpose as :check
;; :castling boolean
;; :from-k
;; :to-k
;; :from-r
;; :to-r - this and the above 3 are coords, used for updating the board following a castle
;; :promotion boolean
;; :player-input string - literal player input
;; :board-packed - string representation of board history, used by DB}

(defn board-lookup
  "Return the string (piece) at coordinates x and y."
  [board [x y]]
  (nth (nth board y) x))

(defn board-lookup-type
  "Look up all the coordinates of pieces 'type'.
  Type is a string representation of the piece, e.g., 'p' for white pawns."
  [board type]
  (->>
   (for [i (range 8)
         :let [row (map-indexed vector (nth board i))
               filtered (filter #(= (second %) type) row)]
         :when (seq filtered)]
     (interleave (map first filtered) (repeat i)))
   flatten
   (partition-all 2)))

(defn update-square
  "Update square on board. If no piece (e.g., 'p') is provided, a blank square is added insteada."
  [board [x y] & piece]
  (->> (assoc (nth board y) x (if piece (first piece) blank-marker))
       (assoc board y)))

(defn update-board-castling [{:keys [board from-k to-k from-r to-r turn]}]
  (let [k-str (if (= turn :white) white-K black-K)
        r-str (if (= turn :white) white-R black-R)]
    (-> (update-square board from-k)
        (update-square from-r)
        (update-square to-k k-str)
        (update-square to-r r-str))))

(defn update-board [{:keys [board from to turn piece piece-str] :as move}]
  (if (:castling move) (update-board-castling move)
      (let [piece-str (if (:promotion move)
                        (type-keyword-lookup (:promotion move) turn)
                        (or
                         piece-str
                         (type-keyword-lookup piece turn)))]
        (-> (update-square board from)
            (update-square to piece-str)))))

;;; Board History

(defn get-board-history! [gameid]
  (db/get-board-history gameid))

(defn pack-board
  "Queries db and adds current board state to previous states.
  row-separator \":\"
  col-separator \" \"
  board-separator \"_\" "
  ([board]
   (->> row
        (str/join ":")
        (for [row board])
        (str/join " ")))
  ([board gameid]
   (let [history (get-board-history! gameid)]
     (->> row
          (str/join ":")
          (for [row board])
          (str/join " ")
          (str history "_")))))

(defn unpack-board [board]
  (->> (str/split board #" ")
       (mapv #(str/split % #":"))))

(defn unpack-history [history]
  (map unpack-board
       (str/split history #"_")))

(defn get-board-state
  ([history] (last (unpack-history history)))
  ([history n]
   (let [hist (unpack-history history)]
     (when (< n (count hist))
       (nth hist n)))))

;;; Helpers
(defn player-pieces [turn]
  (if (= turn :white) white-pieces black-pieces))

(defn opponent-pieces [turn]
  (if (= turn :white) black-pieces white-pieces))

(defn invert-turn [turn]
  (if (= turn :white) :black :white))

;;; Piece Mechanics/validation

(defn valid-to
  "Move is valid if space empty or occupied by opponent piece"
  [{:keys [board to turn]}]
  (let [p (board-lookup board to)]
    (or (= blank-marker p)
        ((opponent-pieces turn) p))))

(defn valid-from
  "Move is valid if 'from' contains the same piece referenced in 'piece'"
  [{:keys [board from turn piece]}]
  (when piece
    (let [p (type-keyword-lookup piece turn)]
      (= p (board-lookup board from)))))

(defn diagonal? [[x1 y1] [x2 y2]]
  (= (abs (- x2 x1))
     (abs (- y2 y1))))

(defn between-squares-h [[x1 y1] [x2 y2]]
  (-> (for [i (apply range (sort (if (= x1 x2) [y1 y2] [x1 x2])))]
        (if (= x1 x2) [x1 i] [i y1]))
      rest))

(defn between-squares-d [[x1 y1] [x2 y2]]
  (when (diagonal? [x1 y1] [x2 y2])
    (let [slope (/ (- y2 y1)
                   (- x2 x1))
          xs (rest (apply range (sort [x1 x2])))
          ys (apply range (sort [y1 y2]))

          ys (if (= slope -1)
               (drop-last (reverse ys))
               (rest ys))]
      (->> (interleave xs ys)
           (partition-all 2)))))

;; Pawn
(defn valid-move-P [{:keys [board from to turn] :as move}]
  (let [opponent-pieces (opponent-pieces turn)
        [x1 y1]         from
        [_ y2]          to
        lookup          (partial board-lookup board)
        starting-points (into #{} (for [i (range 8)]
                                    [i (if (= turn :white) 6 1)]))
        direction       (if (= turn :white) - +)]
    (and
     (and (not (:promotion move)) (not (= y2 (if (= turn :white) 0 7))))
     (= (if (= turn :white) white-P black-P)
        (lookup from))
     (cond
       (= blank-marker (lookup to)) (or (= [x1 (direction y1 1)] to)
                                        (and (starting-points from)
                                             (= [x1 (direction y1 2)] to)))
        
       (opponent-pieces (lookup to)) (or (= [(dec x1) (direction y1 1)] to)
                                         (= [(inc x1) (direction y1 1)] to))
       :else                         false))))

;; Knight
(defn valid-move-N [{:keys [from to]}]
  (let [[x1 y1]         from]
    (or
     (= to [(+ x1 2) (- y1 1)])
     (= to [(- x1 2) (- y1 1)])
     (= to [(+ x1 2) (+ y1 1)])
     (= to [(- x1 2) (+ y1 1)])
     (= to [(+ x1 1) (+ y1 2)])
     (= to [(- x1 1) (+ y1 2)])
     (= to [(+ x1 1) (- y1 2)])
     (= to [(- x1 1) (- y1 2)]))))

;; Rook
(defn valid-move-R [{:keys [board from to turn] :as move}]
  (let [[x1 y1] from
        [x2 y2] to
        lookup  (partial board-lookup board)]
    (when (or (= x1 x2) (= y1 y2))
      (let [between-squares (between-squares-h from to)]
        (or (every? #(= blank-marker %) (map lookup between-squares))
            (empty? between-squares))))))

;; Bishop
(defn valid-move-B [{:keys [board from to]}]
  (let [between-points (between-squares-d from to)
        lookup (partial board-lookup board)]
    ;; Note: 'between-squares-d' checks to see if the points are diagonal
    (and between-points
         (or (empty? between-points)
             (every? #(= blank-marker %) (map lookup between-points))))))

;; Queen
(defn valid-move-Q [move]
  (or (valid-move-R move)
      (valid-move-B move)))

;; King
(defn valid-move-K [{:keys [from to]}]
  (let [[x1 y1] from
        [x2 y2] to]
    (or (and (= 1 (abs (- x2 x1))) (= y2 y1))
        (and (= 1 (abs (- y2 y1))) (= x2 x1))
        (and (= 1 (abs (- x2 x1))) (= 1 (abs (- y2 y1)))))))

(defn valid-m-fn-lookup [board p]
  (case (str/lower-case (board-lookup board p))
    "k" valid-move-K
    "q" valid-move-Q
    "b" valid-move-B
    "r" valid-move-R
    "n" valid-move-N
    "p" valid-move-P))

;; Castle
(defn valid-move-castling [{:keys [board turn castling gameid] :as move}]
  (let [rx1      (if (= castling :kingside) 7 0)
        ry1      (if (= turn :white) 7 0)
        rook     [rx1 ry1]
        new-rook [(if (= castling :kingside) 5 3) ry1]
        king     [4 ry1]
        new-king [(if (= castling :kingside) 6 2) ry1]
        history  (if gameid (unpack-history (get-board-history! gameid)) [])
        lookup   (partial board-lookup board)]
    (when
        (and
         (= (if (= turn :white) white-K black-K) (lookup king))
         (= (if (= turn :white) white-R black-R) (lookup rook))
         (= blank-marker (lookup new-king))
         (= blank-marker (lookup new-rook))
         ;; checking if pieces have moved previously:
         (every? #(= (if (= turn :white) white-R black-R) %)
                 (map #(board-lookup % rook) history))
         (every? #(= (if (= turn :white) white-K black-K) %)
                 (map #(board-lookup % king) history)))
        (-> move
            (assoc :from-k king)
            (assoc :to-k new-king)
            (assoc :from-r rook)
            (assoc :to-r new-rook)))))

;;; Check
(defn check-detection
  "Goes through attackers (turn) pieces, and sees if any can reach the position
  that the defender's king is on (to)"
  [board turn]
  (let [to                     (first (board-lookup-type board (if (= turn :white) black-K white-K)))
        player-piece-positions (reduce (fn [result piece]
                                         (into result (board-lookup-type board piece)))
                                       []
                                       (player-pieces turn))]
    (loop [[p & pieces] player-piece-positions
           check        nil]
      (if-not check
        (when p
          (recur pieces
                 (let [valid-m-fn (valid-m-fn-lookup board p)]
                   (when (valid-m-fn {:board board :from p :to to :turn turn})
                     p))))
        check))))

(defn move-creates-check?
  "Used for move validation. If making a move exposes King to opponent, then it is an invalid move."
  [{:keys [board turn from] :as move}]
  (let [n-turn (invert-turn turn)
        piece-str (board-lookup board from)]
    (-> move
        (assoc :piece-str piece-str)
        update-board
        (check-detection n-turn))))

;;; Checkmate
(defn move-out-of-check?
  "Updates board with defender (turn) possible move, then checks based on attacker positions
  if check still holds."
  [board possible-positions piece-coord turn]
  (let [validate-fn     (valid-m-fn-lookup board piece-coord)
        valid-positions (filter #(validate-fn {:board board :from piece-coord :to %}) possible-positions)
        p-str           (board-lookup board piece-coord)]
    (when valid-positions
      (loop [[v & vs]    valid-positions
             valid-moves []]
        (if-not v (seq valid-moves)
                (if
                    (check-detection
                     (update-board {:board board :from piece-coord :to v :piece-str p-str})
                     (invert-turn turn)) ;; Simulating the attackers check again
                    (recur vs valid-moves)
                    (recur vs (conj valid-moves v))))))))

(defn checkmate-detection [board turn]
  (let [turn               (if (= turn :white) :black :white) ;; simulating next turn
        pieces-coords             (reduce (fn [result piece]
                                            (into result (board-lookup-type board piece)))
                                          []
                                          (player-pieces turn))
        empty-spaces       (board-lookup-type board blank-marker)
        opponent-positions (reduce concat (map #(board-lookup-type board %) (opponent-pieces turn)))
        possible-positions (concat empty-spaces opponent-positions)]
    (loop [[p & ps]  pieces-coords
           checkmate true]
      (if-not checkmate
        :not-checkmate
        (if-not p
          :checkmate
          (recur
           ps
           (when-not (move-out-of-check? board possible-positions p turn)
             checkmate)))))))

;; TODO further validation of castling needed?
(defn validate-move [m]
  (if-not (:castling m)
    (and m
         (valid-to m)
         (valid-from m)
         (not (move-creates-check? m)))
    true))

;;;; Input Handling
;; Algebraic notation:
;; - Nc6
;; - Nxc6
;; - c5 (for pawn)
;; - exd5 (pawn capture)
;; Long Algebraic notation:
;; - e2e4
;; - Rd3xd7

(defn valid-input-string? [input]
  (and (re-find #"[a-zA-Z]" (str (first input)))
       (or (re-find #"\d" (str (last input)))
           (re-find #"[qkbrn]" (str/lower-case (last input))))))

(defn rank-file->coords [[rank file]]
  [(- (int rank) 97) (- 8 (- (int file) 48))])

(defn coords->rank-file [[x y]]
  (let [rank (char (+ x 97))
        file (- 8 y)]
    (apply str [rank file])))

(defn construct-move-from-pawn
  "For pawn input"
  [{:keys [board turn to] :as move}]
  (if (and (not (:promotion move)) (= (second to) (if (= turn :white) 0 7)))
    (assoc move :disambiguation-needed? true)
    (if (:from move) move
        ;; Pawn lookup
        (let [[x2 y2]      to
              direction    (if (= turn :white) + -) ;; looking backward
              target-piece (if (= turn :white) white-P black-P)
              lookup       (partial board-lookup board)]
          (if (= blank-marker (board-lookup board to))
            (cond
              (= (lookup [x2 (direction y2 1)]) target-piece)
              (assoc move :from [x2 (direction y2 1)])
              (= (lookup [x2 (direction y2 2)]) target-piece)
              (assoc move :from [x2 (direction y2 2)])
              :else nil)
            (let [diag-l [(dec x2) (direction y2 1)]
                  diag-r [(inc x2) (direction y2 1)]]
              (cond
                (and (= (lookup diag-l) target-piece) (= (lookup diag-r) target-piece))
                (assoc move :disambiguation-needed? true)
                (= (lookup diag-l) target-piece) (assoc move :from diag-l)
                (= (lookup diag-r) target-piece) (assoc move :from diag-r)
                :else                            nil)))))))

(defn construct-move-from [{:keys [board turn piece from] :as move}]
  (if from move
      (if (= piece :pawn) (construct-move-from-pawn move)
          (let [pos        (board-lookup-type board (type-keyword-lookup piece turn))
                valid-m-fn (case piece
                             :king   valid-move-K
                             :queen  valid-move-Q
                             :bishop valid-move-B
                             :rook   valid-move-R
                             :knight valid-move-N
                             :pawn   valid-move-P)
                pos-f      (filter #(valid-m-fn (assoc move :from %)) pos)]
            (cond
              (empty? pos-f) nil
              (> (count pos-f) 1) (assoc move :disambiguation-needed? true)
              :else (assoc move :from (into [] (first pos-f))))))))

(defn constuct-piece-from [{:keys [board from] :as move}]
  (assoc move :piece-str (board-lookup board from)))

(defn castling? [input]
  (some #{input} ["00" "OO" "000" "OOO"]))

(defn castling-side [input]
  (case input
    "00" :kingside
    "OO" :kingside
    "000" :queenside
    "OOO" :queenside))

;; TODO handle this input 'dxe4' - this is a pawn capturing e4 when there are two pawns (using rank to disambiguate), caused error on input.
;; TODO hanlde an incorrect input like c3456, which tries to match 'c'
(defn parse-input [{:keys [input gameid] :as move}]
  (let [input (str/replace input #"[^a-zA-Z\d]" "")]
    (if (castling? input) (-> (assoc move :castling (castling-side input))
                              valid-move-castling)
        (when (valid-input-string? input)
          (cond
            ;; pawn move, eg, 'd4'
            (= 2 (count input))
            (construct-move-from (-> (assoc move :piece :pawn)
                                     (assoc :to (rank-file->coords input))))

            ;; pawn capture, e.g., 'xd4'
            (and (= 3 (count input)) (= "x" (str/lower-case (first input))))
            (parse-input (assoc move :input (str (rest input))))

            ;; promotion, e.g., 'e8Q'
            (and (= 3 (count input)) (re-find #"[qknrb]" (str/lower-case (last input))))
            (parse-input  (-> (assoc move :promotion (lookup-piece (str (last input))))
                              (assoc :input (subs input 0 2))))

            ;; piece move, e.g., 'Nf3'
            (= 3 (count input))
            (let [to     (rank-file->coords (subs input 1 3))
                  update (fn [piece] (-> (assoc move :piece piece)
                                         (assoc :to to)))]
              (construct-move-from (update (lookup-piece (first input)))))

            ;; piece move capture, e.g., 'Nxf3'
            (and (= 4 (count input)) (= "x" (str/lower-case (second input))))
            (parse-input (assoc move :input (str (subs input 0 1) (subs input 2))))

            ;; long algebraic notation, e.g., 'e2e4'
            (= 4 (count input))
            (let [from (rank-file->coords (subs input 0 2))
                  to   (rank-file->coords (subs input 2 4))]
              (constuct-piece-from (-> (assoc move :from from)
                                       (assoc :to to))))

            ;; long notation capture, e.g., 'e2xe4'
            (and (= 5 (count input)) (= "x" (str/lower-case (nth input 2))))
            (parse-input (-> (assoc move :piece :pawn) (assoc :input (str (subs input 0 2) (subs input 3)))))

            ;; long notation piece, e.g., 'Rd3d7'
            (= 5 (count input))
            (parse-input (-> (assoc move :piece (lookup-piece (first input))) (assoc :input (str (rest input)))))

            ;; long notation piece capture, e.g., 'Rd3xd7'
            (and (= 6 (count input)) (= "x" (str/lower-case (nth input 3))))
            (parse-input (assoc move :input (str (subs input 0 3) (subs input 4))))
            :else nil)))))


;;;; Notation
(defn notate-move [{:keys [piece to castling disambiguation-needed? check checkmate capture promotion from] :as move}]
  (let [check-notation (cond
                         (= checkmate :checkmate) "#"
                         check                    "+"
                         :else                    "")]
    (if castling (str (case castling :kingside "0-0" :queenside "0-0-0")
                      check-notation)
        (let [to   (coords->rank-file to)
              from (coords->rank-file from)]
          (cond
            disambiguation-needed? (str (when (not= piece :pawn)
                                          (type-keyword-lookup piece :black))
                                        from
                                        (when capture "x")
                                        to
                                        check-notation)
            promotion              (str to (type-keyword-lookup promotion :black)
                                        check-notation)
            (and (= piece :pawn)
                 (not capture))    (str to check-notation)
            (= piece :pawn)        (str from "x" to check-notation)
            :else
            (let [piece (type-keyword-lookup piece :black)]
              (str piece (when capture "x") to check-notation)))))))


(defn format-notation-history [gamemoves]
  (let [moves (->> (str/split gamemoves #",")
                   (partition-all 2))]
    (str/join "\n"
              (for [i (range (count moves))
                    :let [m (str (inc i) ". " (str/join " " (nth moves i)))]]
                m))))


(defn notate-pgn [gameid]
  (let [{:chessgames/keys [startdate
                           whiteID
                           blackID
                           winner
                           gamemoves]} (first (db/get-gameinfo gameid))
        location                       "Online, Gemini"
        [date _]                       (str/split startdate #" ")
        [white black]                  (map db/get-username-by-id [whiteID blackID])
        result                         (cond
                                         (= winner "white") "1-0"
                                         (= winner "black") "0-1"
                                         :else              "1/2-1/2")
        moves                          (str (str/replace (format-notation-history gamemoves) #"\n" " ") " " result)
        quote (char 34)
        par-o (char 91)
        par-c (char 93)
        info-fn (fn [label info] (str par-o label " " quote info quote par-c))]

    (str/join "\n"
              [(info-fn "Site" location)
               (info-fn "Date" date)
               (info-fn "White" white)
               (info-fn "Black" black)
               (info-fn "Result" result)
               "\n"
               moves])))

(defn piece-captured?
  "Count number of pieces between board states to see if something was captured.
  Used for move notation purposes"
  [prev-board nxt-board turn]
  (let [piece-count (fn [bd pieces]
                      (reduce (fn [acc piece]
                                (+ acc (count (board-lookup-type bd piece))))
                              0
                              pieces))
        pieces (if (= turn :white) black-pieces white-pieces)]
    (not= (piece-count prev-board pieces) (piece-count nxt-board pieces))))


#_(defn disambiguation-needed?
    "Checks if more than one piece can move to 'to'. Used for notation purposes."
    [{:keys [board turn piece] :as move}]
    (let [valid-m-fn     (fn [piece] (case piece
                                       :king   valid-move-K
                                       :queen  valid-move-Q
                                       :bishop valid-move-B
                                       :rook   valid-move-R
                                       :knight valid-move-N
                                       :pawn   valid-move-P))
          lookup-type    (partial board-lookup-type board)
          lookup         (partial board-lookup board)
          possible-froms (->> (map lookup-type (player-pieces turn))
                              (reduce concat))
          valid-froms    (->>
                          (for [p    possible-froms
                                :let [piece (-> (lookup p) lookup-piece)]]
                            ((valid-m-fn piece) (assoc move :from p)))
                          (filter true?))]
      (not= 1 (count valid-froms))))

(defn disambiguation-needed?
  "Re-runs functions that were used for parsing input and checks for disambiguation flag.
  Not the most efficient way to do this..."
  [{:keys [piece] :as move}]
  (when (not (:castling move))
    (if (= :pawn piece)
      (:disambiguation-needed? (construct-move-from-pawn move))
      (:disambiguation-needed? (construct-move-from move)))))


;;;; Update game state
(defn update-game-record! [req {:keys [board turn gameid] :as move}]
  (let [new-board        (update-board move)
        check-status     (check-detection new-board turn)
        checkmate-status (when check-status (checkmate-detection new-board turn))
        capture?         (piece-captured? board new-board turn)
        disambiguate?    (disambiguation-needed? move)
        move             (-> move
                             (assoc :check check-status)
                             (assoc :checkmate checkmate-status)
                             (assoc :capture capture?)
                             (assoc :board-packed (pack-board new-board gameid))
                             (assoc :disambiguation-needed? disambiguate?))
        notation         (notate-move move)
        move             (assoc move :notation notation)]
    (db/update-chess-game req move)))

;;;; Testing
;; TODO delete these after testing
(def sample-game ["e4" "e5"
                  "Nf3" "d6"
                  "d4" "Bg4"
                  "d4xe5" "Bxf3"
                  "Qxf3" "d6xe5"
                  "Bc4" "Nf6"
                  "Qb3" "Qe7"
                  "Nc3" "c6"
                  "Bg5" "b5"
                  "Nxb5" "c6xb5"
                  "Bxb5+" "Nb8d7"
                  "0-0-0" "Rd8"
                  "Rxd7" "Rxd7"
                  "Rd1" "Qe6"
                  "Bxd7+" "Nxd7"
                  "Qb8+" "Nxb8"
                  "Rd8#"])

(def sample-game-2 ["e4" "c6"
                    "d4" "d5"
                    "Nc3" "xe4"
                    "Nxe4" "Nf6"
                    "Qd3" "e5"
                    "xe5" "Qa5+"
                    "Bd2" "Qxe5"
                    "0-0-0" "Nxe4"
                    "Qd8+" "Kxd8"
                    "Bg5+" "Kc7"
                    "Bd8"])

(def sample-game-3 ["e4" "e5"
                    "Nf3" "Nc6"
                    "Bc4" "Bc5"
                    "b4" "Bxb4"
                    "c3" "Ba5"
                    "d4" "Pxd4"
                    "0-0"])


(comment
  (let [b (atom default-board)
        t (atom :white)
        c (atom 1)]
    (for [move sample-game-3
          :let [m (-> {:board @b :turn @t :input move} parse-input)
                update (update-board m)
                n-turn (if (= @t :white) :black :white)]]
      (do
        (println (str @c ". " (notate-move m)))
        (println "\n")
        (println (when (check-detection update @t) "Check!"))
        (println "\n")
        (println (checkmate-detection update @t))
        (println (draw-board update))
        (println "\n")
        (println "------------------------------------------\n")
        (reset! t n-turn)
        (reset! b update)
        (swap! c inc)))))

;;;; UI
;;; Turn Logic

(defn play-turn [req gameid]
  (let [board (-> (db/get-board-history gameid) get-board-state)
        turn  (db/get-player-type req gameid)]
    (if-not (:query req)
      {:status 10 :meta "Enter your move"}

      (let [move (parse-input {:board board
                               :turn turn
                               :input (:query req)
                               :gameid gameid})
            valid? (validate-move move)]
        (cond
          (:disambiguation-needed? move) {:status 10 :meta "Two pieces can move here, please enter full move, e.g., e2e4"}
          (or (nil? move) (not valid?)) {:status 10 :meta "This move is invalid, please try again"}
          :else
          (do
            (update-game-record! req (assoc move :player-input (:query req)))
            {:status 30 :meta (str root "/game/" gameid)}))))))


(defn resign [req gameid]
  (if-not (:query req)
    {:status 10 :meta "Are you sure you want to resign? [Y/N]"}
    (let [resp (-> (str/lower-case (:query req))
                   (subs 0 1))
          game-redirect {:status 30 :meta (str root "/game/" gameid)}]
      (cond
        (= resp "y") (do (db/resign-game req gameid)
                         game-redirect)
        (= resp "n") game-redirect
        :else {:status 10 :meta "Please enter 'Y' or 'N'"}))))

(defn offer-draw [req gameid]
  (do
   (db/draw-offered req gameid)
   {:status 30 :meta (str root "/game/" gameid)}))

(defn accept-draw [req gameid]
  (do
   (db/draw-accepted req gameid)
   {:status 30 :meta (str root "/game/" gameid)}))

(defn reject-draw [req gameid]
  (do
    (db/draw-rejected req gameid)
    {:status 30 :meta (str root "/game/" gameid)}))

;;; Active Games page

(defn game-summary [game-info]
  (let [startedby (db/get-username-by-id (:chessgames/startedby game-info))]
    (str
     "### Game " (:chessgames/gameid game-info) "\n"
     "Started by " (or startedby "somebody") " on " (:chessgames/startdate game-info) "\n"
     "=> " root "/game/" (:chessgames/gameid game-info) " View Game")))

(defn active-games [req]
  (let [{:keys [player-games open-games running-games]} (db/get-active-games req)
        game-list (fn [games]
                    (if-not (seq games)
                      (str "Nothing here yet.")
                      (str/join break
                                (for [g games]
                                  (game-summary g)))))]
    (->>
     ["# Active Games"
      (str "=> " root " Back")
      "## My Games"
      (game-list player-games)
      (str "=> " root "/start-game Start a new game")
      "## Open Games"
      (game-list open-games)
      "## Running Games"
      (game-list running-games)]
     (str/join break)
     (r/success-response r/gemtext))))



;;; Starting a game

(defn init-game [req colour]
  (let [gameid (-> (random-uuid)
                   str
                   (subs 0 8))
        c (if (= colour "white") :whiteID :blackID)]
    (do (db/init-game req (pack-board default-board) c gameid)
        {:status 30 :meta (str root "/game/" gameid)})))

(defn join-game [req gameid colour]
  (let [c (if (= colour "white") :whiteID :blackID)]
    (do (db/player-join req gameid c)
        {:status 30 :meta (str root "/game/" gameid)})))

(defn start-game-page [req]
  (let [colour (second (:path-args req))]
    (if-not colour
      (->>
       (str "# Start a new game"
            break
            "Choose starting colour: \n"
            "=> start-game/white White\n"
            "=> start-game/black Black"
            break
            (str "=> " root " Chess"))
       (r/success-response r/gemtext))
      (init-game req colour))))

;;; Playing a game

(defn game-page [req gameid board-orientation]
  (let [{:chessgames/keys [whiteID
                           blackID
                           playerturn
                           startdate
                           enddate
                           startedby
                           boardstate
                           checkstate
                           complete
                           winner
                           gamemoves
                           drawstatus
                           resignstatus]} (first (db/get-gameinfo gameid))

        user            (fn [id] (db/get-username-by-id id))
        user-colour     (if (= (db/client-id req) whiteID) "white" "black")
        opponent-colour (if (= user-colour "white") "black" "white")
        last-move       (when gamemoves (last (str/split gamemoves #",")))]
    (->>
     (str
      "# Game " gameid
      break
      "Started by " (user startedby) " on " startdate
      break
      (when enddate (str "Finished on " enddate))
      break
      (when (= complete 1) (str "=> " root "/playback/" gameid " Playback Game"))
      break
      "```\n"
      (cond
        (and (= user-colour "black") (not= board-orientation "default"))
        (-> boardstate get-board-state rotate-board (draw-board :rotate))

        (= board-orientation "rotate")
        (-> boardstate get-board-state rotate-board (draw-board :rotate))

        :else (-> boardstate get-board-state draw-board))
      "\n```"
      break
      (when (and (= checkstate 1) (not= complete 1)) "Check!")
      break
      (cond
        (and (or (not whiteID)
                 (not blackID))
             (= startedby
                (db/client-id req))) "Waiting for other player to join."
        (not whiteID)                (str "=> " root "/join-game/" gameid "/" "white" " Join this game as white")
        (not blackID)                (str "=> " root "/join-game/" gameid "/" "black" " Join this game as black")
        :else
        (if-not (or (= (db/client-id req) whiteID)
                    (= (db/client-id req) blackID))
          (when (= complete 0) (str (user whiteID) "(white) and " (user blackID) " (black) are playing this game. Game ongoing."))
          (cond
            (and (= drawstatus 1)
                 (= user-colour playerturn)) "You have offered a draw, waiting for opponent to accept."
            (= drawstatus 1)                 (str "A draw has been offered, do you accept?\n"
                                                  (str "=> " root "/draw-accept/" gameid " Accept draw\n")
                                                  (str "=> " root "/draw-reject/" gameid " Reject draw"))
            (= drawstatus 2)                 (str "Game tied!")
            (= resignstatus 1)               (str (user (if (= winner "white") blackID whiteID)) " resigned. "
                                                  (user (if (= winner "white") whiteID blackID)) " (" winner ") has won!")
            (= complete 1)                   (str (user (if (= winner "white") whiteID blackID)) " (" winner ") has won!")
            (= playerturn user-colour)
            (str
             (when last-move (str (str/capitalize opponent-colour) " played " last-move "\n"))
             "It's your turn\n"
             "=> " root "/play-turn/" gameid " Play turn\n"
             (when (= 0 drawstatus) (str "=> " root "/draw-offer/" gameid " Offer draw\n"))
             (str "=> " root "/resign/" gameid " Resign game"))
            :else
            (str (str/capitalize playerturn) "'s turn."))))
      break
      (when gamemoves (format-notation-history gamemoves))
      break
      (str "=> " root "/game/" gameid "/"
           (cond
             (and (= board-orientation "")
                  (= user-colour "black"))  "default"
             (and (= board-orientation "")
                  (= user-colour "white"))  "rotate"
             (= board-orientation "rotate") "default"
             :else                          "rotate")
           " Rotate board")
      break
      "=> " root " Back")
     (r/success-response r/gemtext))))

(defn game-playback-page [_ gameid move]
  (let [{:chessgames/keys
         [turncount
          boardstate
          gamemoves]} (first (db/get-gameinfo gameid))

        move         (if move (parse-long move) (dec turncount))
        move         (if (> move (dec turncount)) (dec turncount) move)
        move         (if (< move 1) 1 move)
        current-move (nth (str/split gamemoves #",") (dec move))]
    (->>
     [(str "# Game " gameid " Playback")
      (str "Move " move ": " (if (even? move) "Black" "White") " played " current-move)
      (str "=> " root "/playback/" gameid "/" (dec move)" Previous Move")
      (str "=> " root "/playback/" gameid "/" (inc move)" Next Move")
      (str "=> " root "/playback/" gameid "/1 First Move")
      "```"
      (-> boardstate (get-board-state move) draw-board)
      "```"
      "## PGN Data for Game"
      "You can copy/paste the below notation for the game into another chess viewer, for example:"
      "=> https://lichess.org/paste Lichess - Game Viewer"
      (notate-pgn gameid)
      (str "=> " root " Back to Chess")
      "=> / Home"]
     (str/join break)
     (r/success-response r/gemtext))))

;;; Main Page

;; TODO completed games?
(defn main-page [req]
  (let [user (db/get-username req)]
    (->>
     (str "# Chess"
          break
          "=> / Home"
          break
          (if-not user
            (str "=> " root "/name/ Enter your name")
            (str "=> " root "/active-games Active Games\n"
                 "=> " root "/start-game Start a new Game"
                 break
                 "Signed in as " user))
          break
          (slurp "static/partials/chess_info"))
     (r/success-response r/gemtext))))

;;;; Main/routes

(defn main [req]
  (if-not (:client-cert req)
    (reg/register-user)

    (let [[r gameid game-r] (:path-args req)
          route             (or r "/")]
      (case route
        "/"            (main-page req)
        "name"         (reg/register-name req root)
        "active-games" (active-games req)
        "start-game"   (start-game-page req)
        "join-game"    (join-game req gameid game-r)
        "play-turn"    (play-turn req gameid)
        "resign"       (resign req gameid)
        "draw-offer"   (offer-draw req gameid)
        "draw-accept"  (accept-draw req gameid)
        "draw-reject"  (reject-draw req gameid)
        "game"         (game-page req gameid (or game-r ""))
        "playback"     (game-playback-page req gameid game-r)
        (r/success-response r/gemtext "Nothing here")))))
