(require '[space-age.db :as db])
(require '[space-age.responses :as r])
(require '[space-age.user-registration :as reg])
(require '[clojure.string :as str])

(def root "/src/app/chess")
(def break "\n\n")

;; Pieces

(def blank-marker (str (char 183)))

;; (def white-K (str (char 9812)))
;; (def white-Q (str (char 9813)))
;; (def white-R (str (char 9814)))
;; (def white-B (str (char 9815)))
;; (def white-Kn (str (char 9816)))
;; (def white-P (str (char 9817)))

;; (def black-K (str (char 9818)))
;; (def black-Q (str (char 9819)))
;; (def black-R (str (char 9820)))
;; (def black-B (str (char 9821)))
;; (def black-Kn (str (char 9822)))
;; (def black-P (str (char 9823)))

;; (def default-board
;;   [[black-R black-Kn black-B black-Q black-K black-B black-Kn black-R]
;;    (into [] (repeat 8 black-P))
;;    (into [] (repeat 8 blank-marker))
;;    (into [] (repeat 8 blank-marker))
;;    (into [] (repeat 8 blank-marker))
;;    (into [] (repeat 8 blank-marker))
;;    (into [] (repeat 8 white-P))
;;    [white-R white-Kn white-B white-Q white-K white-B white-Kn white-R]])

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
;; {:board board-state
;; :from coords-from (e.g., [0 0])
;; :to coords-to (e.g., [0 1])
;; :turn e.g., :white
;; :piece piece-name e.g., :king
;; :piece-str piece-string e.g., "k"}

(defn board-lookup [board [x y]]
  (nth (nth board y) x))

(defn board-lookup-type [board type]
  (->>
   (for [i (range 8)
         :let [row (map-indexed vector (nth board i))
               filtered (filter #(= (second %) type) row)]
         :when (seq filtered)]
     (interleave (map first filtered) (repeat i)))
   flatten
   (partition-all 2)))

#_(defn occupied? [board [x y]]
    (not= blank-marker (board-lookup board [x y])))

(defn update-square [board [x y] & piece]
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


;; Board DB Interaction

;; For testing
#_(def board-store (atom nil))


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
  ([history n] (when (< n (count history))
                 (nth history n))))







(declare check-detection)

(defn player-pieces [turn]
  (if (= turn :white) white-pieces black-pieces))

(defn opponent-pieces [turn]
  (if (= turn :white) black-pieces white-pieces))

(defn valid-to
  "Move is valid if space empty or occupied by opponent piece"
  [lookup to turn]
  (or (= blank-marker (lookup to))
      ((opponent-pieces turn) (lookup to))))

(defn diagonal? [[x1 y1] [x2 y2]]
  (= (abs (- x2 x1))
     (abs (- y2 y1))))

(defn invert-turn [turn]
  (if (= turn :white) :black :white))

(defn move-creates-check? [{:keys [board turn from] :as move}]
  (let [n-turn (invert-turn turn)
        piece-str (board-lookup board from)]
    (-> move
        (assoc :piece-str piece-str)
        update-board
        (check-detection n-turn))))

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
     (and (not (:promotion move)) (not (= y2 (if (= turn :white) 0 7)))) ;; FIXME - check if this is actually needed here...
     (= (if (= turn :white) white-P black-P)
        (lookup from))
     (cond
       (= blank-marker (lookup to)) (or (= [x1 (direction y1 1)] to)
                                        (and (starting-points from)
                                             (= [x1 (direction y1 2)] to)))
        
       (opponent-pieces (lookup to)) (or (= [(dec x1) (direction y1 1)] to)
                                         (= [(inc x1) (direction y1 1)] to))
       :else                         false)
     (not (move-creates-check? move)))))


;; Knight
(defn valid-move-N [{:keys [board from to turn] :as move}]
  (let [[x1 y1]         from
        lookup          (partial board-lookup board)]
    (and
     (= (if (= turn :white) white-N black-N)
        (lookup from))
     (or
      (= to [(+ x1 2) (- y1 1)])
      (= to [(- x1 2) (- y1 1)])
      (= to [(+ x1 2) (+ y1 1)])
      (= to [(- x1 2) (+ y1 1)])
      (= to [(+ x1 1) (+ y1 2)])
      (= to [(- x1 1) (+ y1 2)])
      (= to [(+ x1 1) (- y1 2)])
      (= to [(- x1 1) (- y1 2)]))
     (valid-to lookup to turn)
     (not (move-creates-check? move)))))

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

;; Rook
(defn valid-move-R [{:keys [board from to turn] :as move}]
  (let [[x1 y1] from
        [x2 y2] to
        lookup  (partial board-lookup board)]
    (when (or (= x1 x2) (= y1 y2))
      (let [between-squares (between-squares-h from to)]
        (and
         (= (if (= turn :white) white-R black-R)
            (lookup from))
         (or (every? #(= blank-marker %) (map lookup between-squares))
             (empty? between-squares))
         (valid-to lookup to turn)
         (not (move-creates-check? move)))))))
          

;; Bishop
(defn valid-move-B [{:keys [board from to turn] :as move}]
  (let [between-points (between-squares-d from to)
        lookup (partial board-lookup board)]
    (and between-points
         (or (every? #(= blank-marker %) (map lookup between-points))
             (empty? between-points))
         (valid-to lookup to turn)
         (not (move-creates-check? move)))))
         

;; Queen
(defn valid-move-Q [{:keys [board from to turn] :as move}]
  (let [[x1 y1] from
        [x2 y2] to
        lookup (partial board-lookup board)
        between-squares (if (or (= x1 x2)
                                (= y1 y2))
                          (between-squares-h from to)
                          (between-squares-d from to))]
    (and (= (if (= turn :white) white-Q black-Q)
            (lookup from))
         between-squares
         (or (empty? between-squares)
             (every? #(= blank-marker %) (map lookup between-squares)))
         (valid-to lookup to turn)
         (not (move-creates-check? move)))))

;; King
(defn valid-move-K [{:keys [board from to turn] :as move}]
  (let [[x1 y1] from
        [x2 y2] to
        lookup (partial board-lookup board)]
    (and (= (if (= turn :white) white-K black-K) (lookup from))
         (or (and (= 1 (abs (- x2 x1))) (= y2 y1))
             (and (= 1 (abs (- y2 y1))) (= x2 x1))
             (and (= 1 (abs (- x2 x1))) (= 1 (abs (- y2 y1)))))
         (valid-to (partial board-lookup board) to turn)
         (not (move-creates-check? move)))))

(defn valid-m-fn-lookup [board p]
  (case (str/lower-case (board-lookup board p))
    "k" valid-move-K
    "q" valid-move-Q
    "b" valid-move-B
    "r" valid-move-R
    "n" valid-move-N
    "p" valid-move-P))

(defn valid-move-castling [{:keys [board turn castling] :as move}]
  (let [rx1      (if (= castling :kingside) 7 0)
        ry1      (if (= turn :white) 7 0)
        rook     [rx1 ry1]
        new-rook [(if (= castling :kingside) 5 3) ry1]
        king     [4 ry1]
        new-king [(if (= castling :kingside) 6 2) ry1]
        history  (unpack-history)
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

(comment
  (valid-move-castling
   (parse-input {:board (-> blank-board
                            (update-square [4 7] "k")
                            (update-square [7 7] "r"))
                 :turn :white}
                "0-0")))
           

(comment
  (valid-move-R {:board (-> (update-square blank-board [0 0] "R")
                            (update-square [0 1] "p"))
                 :from [0 0] :to [0 1] :turn :black})
  (valid-move-N {:board default-board :from [2 0] :to [3 2] :turn :black})
  (valid-move-R {:board default-board :from [2 2] :to [6 3] :turn :black})
  (valid-move-P {:board default-board :from [2 1] :to [1 2] :turn :black}))
  


;; Check

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
        (do
          (println "Check!")
          check)))))

;; Checkmate

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

;; FIXME
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


(comment
  (checkmate-detection

   (-> blank-board
       (update-square [0 3] "K")
       (update-square [2 3] "q")
       (update-square [7 7] "k")
       (update-square [2 1] "n")
       (update-square [4 0] "b")
       (update-square [2 7] "Q"))
   :white)
  (check-detection (-> blank-board
                       (update-square [0 3] "K")
                       (update-square [1 2] "q")) :white))


(comment
  (draw-board
   (update-board {:board default-board
                  :from [6 7]
                  :to [5 5]
                  :piece "n"
                  :capture false})))

;; Input Handling
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

(defn parse-input [move input]
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
            (parse-input move (str (rest input)))

            ;; promotion, e.g., 'e8Q'
            (and (= 3 (count input)) (re-find #"[qknrb]" (str/lower-case (last input))))
            (parse-input  (assoc move :promotion (lookup-piece (str (last input))))
                          (subs input 0 2))

            ;; piece move, e.g., 'Nf3'
            (= 3 (count input))
            (let [to     (rank-file->coords (subs input 1 3))
                  update (fn [piece] (-> (assoc move :piece piece)
                                         (assoc :to to)))]
              (construct-move-from (update (lookup-piece (first input)))))

            ;; piece move capture, e.g., 'Nxf3'
            (and (= 4 (count input)) (= "x" (str/lower-case (second input))))
            (parse-input move (str (subs input 0 1) (subs input 2)))

            ;; long algebraic notation, e.g., 'e2e4'
            (= 4 (count input))
            (let [from (rank-file->coords (subs input 0 2))
                  to   (rank-file->coords (subs input 2 4))]
              (constuct-piece-from (-> (assoc move :from from)
                                       (assoc :to to))))

            ;; long notation capture, e.g., 'e2xe4'
            (and (= 5 (count input)) (= "x" (str/lower-case (nth input 2))))
            (parse-input (assoc move :piece :pawn) (str (subs input 0 2) (subs input 3)))

            ;; long notation piece, e.g., 'Rd3d7'
            (= 5 (count input))
            (parse-input (assoc move :piece (lookup-piece (first input))) (str (rest input)))

            ;; long notation piece capture, e.g., 'Rd3xd7'
            (and (= 6 (count input)) (= "x" (str/lower-case (nth input 3))))
            (parse-input move (str (subs input 0 3) (subs input 4)))
            :else nil)))))

;; TODO - Fix pawn capture notation
(defn notate-move [{:keys [piece to castling disambiguation-needed? check checkmate capture promotion]}]
  (let [to (coords->rank-file to)]
    (cond
      disambiguation-needed?              (println "TODO notate move")
      castling                            (case castling
                                            :kingside  "0-0"
                                            :queenside "0-0-0")
      promotion                           (str to (type-keyword-lookup promotion :black))
      (and (= piece :pawn) (not capture)) to
      :else
      (let [piece          (type-keyword-lookup piece :black)
            check-notation (cond
                             check     "+"
                             checkmate "#"
                             :else     "")]
        (str piece (when capture "x") to check-notation)))))

(defn piece-captured?
  "Count number of pieces between board states to see if something was captured."
  [prev-board nxt-board turn]
  (let [piece-count (fn [bd pieces]
                      (reduce (fn [acc piece]
                                (+ acc (count (board-lookup-type bd piece))))
                              0
                              pieces))
        pieces (if (= turn :white) black-pieces white-pieces)]
    (not= (piece-count prev-board pieces) (piece-count nxt-board pieces))))


(defn update-game-record! [{:keys [board turn] :as move} gameid]
  (let [new-board        (update-board move)
        check-status     (check-detection new-board turn)
        checkmate-status (when check-status (checkmate-detection new-board turn))
        capture?         (piece-captured? board new-board turn)
        move             (-> move
                             (assoc :check check-status)
                             (assoc :checkmate checkmate-status)
                             (assoc :capture capture?)
                             (assoc :board-packed (pack-board new-board gameid)))
        notation         (notate-move move)
        move             (assoc move :notation notation)]
    (db/update-chess-game move gameid)))



(comment
  ;; Testing promotion
  (->
   (parse-input
    {:board
     (-> blank-board
         (update-square [0 1] "p"))
     :turn :white}
    "a8Q")
   (update-board)))


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

(comment
  (let [b (atom default-board)
        t (atom :white)
        c (atom 1)]
    (for [move sample-game
          :let [m (-> {:board @b :turn @t} (parse-input move))
                update (update-board m)
                n-turn (if (= @t :white) :black :white)]]
      (do
        (println m)
        (println "\n")
        (println (str @c ". " move " : "))
        (println "\n")
        (println (draw-board update))
        (println "\n")
        (println "------------------------------------------\n")
        (reset! t n-turn)
        (reset! b update)
        (swap! c inc)))))



;; Turn logic

(defn play-turn [req gameid]
  (let [board (-> (db/get-board-history gameid) get-board-state)
        turn  (db/get-player-type req gameid)]
    (if-not (:query req)
      {:status 10 :meta "Enter your move"}

      (let [move (parse-input {:board board :turn turn} (:query req))]
        (cond
          (:disambiguation-needed? move) {:status 10 :meta "Two pieces can move here, please enter full move, e.g., e2e4"}
          (nil? move) {:status 10 :meta "This move is invalid, please try again"}
          :else
          (do
            (update-game-record! (assoc move :player-input (:query req)) gameid)
            {:status 30 :meta (str root "/game/" gameid)}))))))



;; Active Games page

(defn game-summary [game-info]
  (let [startedby (db/get-username-by-id (:chessgames/startedby game-info))]
    (str
     "### Game " (:chessgames/gameid game-info) "\n"
     "Started by " (or startedby "somebody") " on " (:chessgames/startdate game-info) "\n"
     "=> " root "/game/" (:chessgames/gameid game-info) " View Game")))

;; TODO message when section is empty, and option to start game from here.
(defn active-games [req]
  (let [{:keys [player-games open-games running-games]} (db/get-active-games req)]
    (->>
     (str
      "# Active Games"
      break
      "## My Games"
      break
      (str/join break
                (for [g player-games]
                  (game-summary g)))
      break
      "## Open Games"
      break
      (str/join break
                (for [g open-games]
                  (game-summary g)))
      break
      "## Running Games"
      break
      (str/join break
                (for [g running-games]
                  (game-summary g)))
      break)
     (r/success-response r/gemtext))))



;; Starting a game

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
            (str "=> " root " Go back"))
       (r/success-response r/gemtext))
      (init-game req colour))))

;; Playing a game

(defn game-page [req gameid]
  (let [{:chessgames/keys [whiteID
                           blackID
                           playerturn
                           startdate
                           startedby
                           boardstate
                           checkstate
                           complete
                           winner
                           gamemoves]} (first (db/get-gameinfo gameid))

        user        (fn [id] (db/get-username-by-id id))
        user-colour (if (= (db/client-id req) whiteID) "white" "black")
        opponent-colour (if (= user-colour "white") "black" "white")
        last-move   (last (str/split gamemoves #","))]
    (->>
     (str
      "# Game " gameid
      break
      "Started by " (user startedby) " on " startdate
      break
      "```\n"
      (-> boardstate get-board-state draw-board)
      "\n```"
      break
      (if (= complete 1)
        (str (user (if (= winner "white") whiteID blackID)) " (" winner ")" " has won!")
        (cond
          winner                                 (str winner " wins!")
          (and (or (not whiteID) (not blackID))
               (= startedby (db/client-id req))) "Waiting for other player to join."
          (not whiteID)                          (str "=> " root "/join-game/" gameid "/" "white" " Join this game as white")
          (not blackID)                          (str "=> " root "/join-game/" gameid "/" "black" " Join this game as black")
          (= playerturn user-colour)             (str
                                                  (when (= checkstate 1) "Check! \n")
                                                  opponent-colour " played " last-move "\n"
                                                  "It's your turn\n"
                                                  "=> " root "/play-turn/" gameid " Play turn")
          :else                                  (str playerturn "'s turn.")))
      break
      "=> " root " Back")

     (r/success-response r/gemtext))))

;; Main Page

(defn main-page [req]
  (->>
   (str "# Chess"
        break
        "=> / Home"
        break
        "=> " root "/active-games Active Games\n"
        "=> " root "/start-game Start a new Game\n")
   (r/success-response r/gemtext)))


;; Main/routes




(defn main [req]
  (if-not (:client-cert req)
    (reg/register-user)

    (let [route (or (first (:path-args req)) "/")]
      (case route
        "/"            (main-page req)
        "name"         (reg/register-name req root)
        "active-games" (active-games req)
        "start-game"   (start-game-page req)
        "join-game"    (join-game req (second (:path-args req)) (last (:path-args req)))
        "play-turn"    (play-turn req (second (:path-args req)))
        "game"         (game-page req (second (:path-args req)))
        (r/success-response r/gemtext "Nothing here")))))
