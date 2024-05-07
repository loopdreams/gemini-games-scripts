(require '[space-age.db :as db])
(require '[space-age.responses :as r])
(require '[space-age.user-registration :as reg])
(require '[clojure.string :as str])

(def root "/src/app/tic_tac_toe")
(def break "\n\n")

(def x-marker (char 9813))
(def o-marker (char 9819))

(def default-board [["-" "-" "-"]
                    ["-" "-" "-"]
                    ["-" "-" "-"]])

(def blank-marker "-")

;; Game logic
(defn row-check [row]
  (when-not (some #{blank-marker} row)
    (apply = row)))

(defn horizontals [board]
  (some true?
        (map row-check board)))

(defn verticals [board]
  (loop [b board
         result []]
    (if (empty? (first b)) (some true? result)
        (recur (map rest b) (conj result (row-check (map first b)))))))

(defn diagonals [board]
  (let [[[a _ x] [_ b _] [y _ c]] board]
    (or (row-check [a b c])
        (row-check [x b y]))))

(defn win? [board]
  (or (horizontals board)
      (diagonals board)
      (verticals board)))

(defn pack-board
  "For storing in sql row. Cell separators are ':' and row separators are ' '"
  [board]
  (->> (map #(str/join ":" %) board)
      (str/join " ")))

(defn unpack-board [board-str]
  (->>
   (str/split board-str #" ")
   (map #(str/split % #":"))))

(defn update-board [board [x y] player gameid]
  (let [marker    (if (= player :white) x-marker o-marker)
        board     (into [] board)
        new-board (->> (assoc (nth board y) x marker)
                       (assoc board y))]
    (db/update-board! gameid (pack-board new-board))))

;; Input parsing
(defn free-space? [board [x y]]
  (= (nth (nth board y) x) blank-marker))

(defn get-row-col-input
  "If nil, input is invalid"
  [input]
  (let [remove-blanks (str/replace input #" " "")
        row           (re-find #"\d" remove-blanks)
        row           (when row (parse-long row))
        col           (re-find #"[abc]" (str/lower-case remove-blanks))]
    (when (and col row (<= 1 row 3))
      (let [col-idx (case col
                      "a" 0
                      "b" 1
                      "c" 2
                      nil)
            row-idx (dec row)]
        [col-idx row-idx]))))
  

(comment
  (get-row-col-input "a2")
  (free-space? default-board [0 1]))

;; Turn logic
(defn play-turn [req gameid]
  (let [board (-> (db/get-gameinfo gameid) first :games/boardstate unpack-board)
        player (db/get-player-type req gameid)]
    (if-not (:query req)
      {:status 10 :meta "Enter coordinates"}

      (let [input (get-row-col-input (:query req))]
        (if-not input
          {:status 10 :meta "Incorrect input, please try again"}

          (if-not (free-space? board input)
            {:status 10 :meta "This space is not free, please try again"}

            (do
              (update-board board input player gameid)
              {:status 30 :meta (str root "/game/" gameid)})))))))


;; Make board
(defn make-board [board-str]
  (unpack-board board-str))

(defn draw-board [rows]
  (let [x-border ["a" "b" "c"]
        y-border ["1" "2" "3"]
        x-border-format (str "    " (str/join "   " x-border))]
    (str "```\n"
         (str x-border-format
              "\n"
              (->>
               (for [i (range (count rows))
                     :let [border (nth y-border i)
                           r (str "| " (str/join " " (interleave (nth rows i) (repeat "|"))))]]
                 (str border " " r " " border))
               (str/join "\n"))
              "\n"
              x-border-format
              "\n")
         "\n```")))


(defn game-summary [game-info]
  (let [startedby (db/get-username-by-id (:games/startedby game-info))]
    (str
     "### Game " (:games/gameid game-info) "\n"
     "Started by " (or startedby "somebody") " on " (:games/startdate game-info) "\n"
     "=> " root "/game/" (:games/gameid game-info) " View Game")))

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
  (let [{:games/keys [whiteID
                      blackID
                      playerturn
                      startdate
                      startedby
                      boardstate
                      complete
                      winner]} (first (db/get-gameinfo gameid))
        user (fn [id] (db/get-username-by-id id))
        user-colour (if (= (db/client-id req) whiteID) "white" "black")]
    (->>
     (str
      "# Game " gameid
      break
      "Started by " (user startedby) " on " startdate
      break
      (-> boardstate make-board draw-board)
      break
      (if (= complete 1)
        (str (user winner) " has won!")
        (cond
          (and (or (not whiteID) (not blackID)) (= startedby (db/client-id req))) "Waiting for other player to join."
          (not whiteID) (str "=> " root "/join-game/" gameid "/" "white" " Join this game as white")
          (not blackID) (str "=> " root "/join-game/" gameid "/" "black" " Join this game as black")
          (= playerturn user-colour) (str "It's your turn\n"
                                          "=> " root "/play-turn/" gameid " Play turn")
          :else (str playerturn "'s turn."))))
     
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
