(require '[space-age.db :as db])
(require '[space-age.responses :as r])
(require '[space-age.user-registration :as reg])
(require '[clojure.string :as str])

(def root "/src/gemini_games/tic_tac_toe")
(def break "\n\n")

(def x (char 9813))
(def o (char 9819))

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


(defn active-games "TODO" [req] nil)
(defn my-games "TODO" [req] nil)
(defn start-game "TODO" [req] nil)

(defn game-page [req gameid]
  (->>
   (str gameid break
        (draw-board [[x o " "] [" " x " "] [o " " " "]]))
   (r/success-response r/gemtext)))

(defn main-page [req]
  (->>
   (str "# Chess"
        break
        "=> / Home"
        break
        "=> " root "/my-games My Games\n"
        "=> " root "/active-games Active Games\n"
        "=> " root "/start-game Start a new Game\n"
        "=> " root "/game/0001 Test Game")
   (r/success-response r/gemtext)))


(defn main [req]
  (if-not (:client-cert req)
    (reg/register-user)

    (let [route (or (first (:path-args req)) "/")]
      (case route
        "/"            (main-page req)
        "name"         (reg/register-name req root)
        "active-games" (active-games req)
        "my-games"     (my-games req)
        "start-game"   (start-game req)
        "game"         (game-page req (second (:path-args req)))
        (r/success-response r/gemtext "Nothing here")))))
