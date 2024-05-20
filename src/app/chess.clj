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
  (-> (update-square board from)
      (update-square to piece)))

(defn pack-board
  "Queries db and adds current board state to previous states.
  row-separator \":\"
  col-separator \" \"
  board-separator \"_\" "
  [board]
  (let [history "TODO"]
    (str history "_"
         (str/join " "
                   (for [row board]
                     (str/join ":" row))))))

(defn unpack-board [board]
  (->> (str/split board #" ")
       (mapv #(str/split % #":"))))

(defn unpack-history []
  (let [history (pack-board default-board-alt)
        boards (str/split history #"_")]
    (map unpack-board boards)))

(defn get-board-state
  ([] (last (unpack-history)))
  ([n] (let [history (unpack-history)]
         (when (< n (count history))
           (nth history n)))))

;; Valid moves for:
;; - DONE Pawn
;; - DONE Rook
;; - DONE Bishop
;; - DONE Knight
;; - WAIT King
;; - DONE Queen
;; - DONE Castling
;; - CANCELLED Promotion (covered in pawn validation)


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

(defn valid-move-castle [{:keys [board turn castling]}]
  (let [rx1      (if (= castling :kingside) 7 0)
        ry1      (if (= turn :white) 7 0)
        rook     [rx1 ry1]
        new-rook [(if (= castling :kingside) 5 3) ry1]
        king     [4 ry1]
        new-king [(if (= castling :kingside) 6 2) ry1]
        history  (unpack-history)
        lookup   (partial board-lookup board)]
    (when (and (= blank-marker (lookup new-king))
               (= blank-marker (lookup new-rook))
               ;; checking if pieces have moved previously:
               (every? #(= (if (= turn :white) white-R-alt black-R-alt) %)
                       (map #(board-lookup % rook) history))
               (every? #(= (if (= turn :white) white-K-alt black-K-alt) %)
                       (map #(board-lookup % king) history)))
      "TODO handle castling board update")))
           

(comment
  (valid-move-R {:board (-> (update-square blank-board [0 0] "R")
                            (update-square [0 1] "p"))
                 :from [0 0] :to [0 1] :turn :black})
  (valid-move-N {:board default-board-alt :from [2 0] :to [3 2] :turn :black})
  (valid-move-R {:board default-board-alt :from [2 2] :to [6 3] :turn :black})
  (valid-move-P {:board default-board-alt :from [2 1] :to [1 2] :turn :black}))
  


;; TODO Castling
;; TODO New pieces

;; Check


(defn board-lookup-type [board type]
  (->>
   (for [i (range 8)
         :let [row (map-indexed vector (nth board i))
               filtered (filter #(= (second %) type) row)]
         :when (seq filtered)]
     (interleave (map first filtered) (repeat i)))
   flatten
   (partition-all 2)))

(defn valid-m-fn-lookup [board p]
  (case (str/lower-case (board-lookup board p))
    "k" valid-move-K
    "q" valid-move-Q
    "b" valid-move-B
    "r" valid-move-R
    "n" valid-move-N
    "p" valid-move-P))


(defn check-detection
  "Goes through attackers (turn) pieces, and sees if any can reach the position
  that the defender's king is on (to)"
  [board turn]
  (let [to                     (first (board-lookup-type board (if (= turn :white) black-K-alt white-K-alt)))
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

;; Checkmate

(defn move-out-of-check?
  "Updates board with defender (turn) possible move, then checks based on attacker positions
  if check still holds."
  [board possible-positions piece turn]
  (let [validate-fn     (valid-m-fn-lookup board piece)
        valid-positions (filter #(validate-fn {:board board :from piece :to %}) possible-positions)
        p-str           (board-lookup board piece)]
    (when valid-positions
      (loop [[v & vs]    valid-positions
             valid-moves []]
        (if-not v (seq valid-moves)
                (if
                    (check-detection
                     (update-board board {:from piece :to v :piece p-str})
                     (if (= turn :white) :black :white)) ;; Simulating the attackers check again
                    (recur vs valid-moves)
                    (recur vs (conj valid-moves v))))))))

;; FIXME - the 'turn' logic here is causing this to error...
(defn checkmate-detection [board turn]
  (let [turn               (if (= turn :white) :black :white) ;; simulating next turn
        pieces             (reduce (fn [result piece]
                                     (into result (board-lookup-type board piece)))
                                   []
                                   (player-pieces turn))
        empty-spaces       (board-lookup-type board blank-marker)
        opponent-positions (reduce concat (map #(board-lookup-type board %) (opponent-pieces turn)))
        possible-positions (concat empty-spaces opponent-positions)]
    (loop [[p & ps]  pieces
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
   (update-board default-board-alt {:from [6 7]
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

(defn construct-move-from-pawn
  "For pawn input"
  [{:keys [board turn to] :as move}]
  (if (:from move) move
      ;; Pawn lookup
      (let [[x2 y2]      to
            direction    (if (= turn :white) + -)
            target-piece (if (= turn :white) white-P-alt black-P-alt) ;; looking backward
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
              "TODO Disambiguation step"
              (= (lookup diag-l) target-piece) (assoc move :from diag-l)
              (= (lookup diag-r) target-piece) (assoc move :from diag-r)
              :else                            nil))))))


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

  
(defn construct-move-from [{:keys [board turn piece to] :as move}]
  (if (:from move) move
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
            (if (> (count pos-f) 1) "TODO Disambiguation step"
                (assoc move :from (into [] (first pos-f))))))))
          
(defn constuct-piece-from [{:keys [board from] :as move}]
  (assoc move :piece (board-lookup board from)))

(defn lookup-piece [letter]
  (case (str/lower-case letter)
    "k" :king
    "q" :queen
    "b" :bishop
    "r" :rook
    "n" :knight
    "p" :pawn))

(defn castling? [input]
  (some #{input} ["00" "OO" "000" "OOO"]))

(defn castling-side [input]
  (case input
    "00" :kingside
    "OO" :kingside
    "000" :queenside
    "OOO" :queenside))

(defn parse-input [input move]
  (let [input (str/replace input #"[^a-zA-Z\d]" "")]
    (if (castling? input) (assoc move :castling (castling-side input))
        (when (valid-input-string? input)
          (cond
            ;; pawn move, eg, 'd4'
            (= 2 (count input))
            (construct-move-from (-> (assoc move :piece :pawn)
                                     (assoc :to (rank-file->coords input))))

            ;; pawn capture, e.g., 'xd4'
            (and (= 3 (count input)) (= "x" (str/lower-case (first input))))
            (parse-input (str (rest input)) move)

            ;; promotion, e.g., 'e8Q'
            (and (= 3 (count input)) (re-find #"[qknrb]" (str/lower-case (last input))))
            (parse-input (subs input 0 2) (assoc move :promotion (lookup-piece (str (last input)))))

            ;; piece move, e.g., 'Nf3'
            (= 3 (count input))
            (let [to     (rank-file->coords (subs input 1 3))
                  update (fn [piece] (-> (assoc move :piece piece)
                                         (assoc :to to)))]
              (construct-move-from (update (lookup-piece (first input)))))

            ;; piece move capture, e.g., 'Nxf3'
            (and (= 4 (count input)) (= "x" (str/lower-case (second input))))
            (parse-input (str (subs input 0 1) (subs input 2)) move)

            ;; long algebraic notation, e.g., 'e2e4'
            (= 4 (count input))
            (let [from (rank-file->coords (subs input 0 2))
                  to   (rank-file->coords (subs input 2 4))]
              (constuct-piece-from (-> (assoc move :from from)
                                       (assoc :to to))))

            ;; long notation capture, e.g., 'e2xe4'
            (and (= 5 (count input)) (= "x" (str/lower-case (nth input 2))))
            (parse-input (str (subs input 0 2) (subs input 3)) move)

            ;; long notation piece, e.g., 'Rd3d7'
            (= 5 (count input))
            (parse-input (str (rest input)) (assoc move :piece (lookup-piece (first input))))

            ;; long notation piece capture, e.g., 'Rd3xd7'
            (and (= 6 (count input)) (= "x" (str/lower-case (nth input 3))))
            (parse-input (str (subs input 0 3) (subs input 4)) move)
            :else nil)))))


(comment
  (parse-input "f8Q" {:board default-board-alt :turn :white})
  (parse-input "a8=Q" {:board
                       (-> blank-board
                           (update-board {:from [0 1] :to [0 1] :piece "p"}))
                       :turn :white}))

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
