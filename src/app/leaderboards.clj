(require '[space-age.leaderboards :as lb])
(require '[space-age.responses :as r])
(require '[clojure.string :as str])

(def break "\n\n")

(defn pad-row-r [data max-len]
  (let [data (if-not data "" (str data))]
    (str data (apply str (repeat (- max-len (count data)) " ")))))

(defn pad-row-l [data max-len]
  (let [data (if-not data "" (str data))]
    (str (apply str (repeat (- max-len (count data)) " ")) data)))

(def header ["Name" "Score"])

(defn draw-row [[name score] name-len score-len]
  (str "|"
       (pad-row-r name name-len) "|"
       (if (= score (second header))
         (pad-row-r score score-len)
         (pad-row-l score score-len)) "|"))

(defn draw-leaderboard [data]
  (let [entries       (for [entry data]
                        [(:name entry) (:score entry)])
        name-col-len  (inc (last (sort (map (comp count :name) data))))
        score-col-len (inc (count (last header)))
        divider-name  (apply str (repeat name-col-len "-"))
        divider-score (apply str (repeat score-col-len "-"))]
    (->>
     entries
     (into [header [divider-name divider-score]])
     (map #(draw-row % name-col-len score-col-len))
     (str/join "\n"))))

(defn wordle-leaderboard []
  (draw-leaderboard (lb/wordle-leaderboard-data)))

(defn wordle-daily-leaderboard []
  (draw-leaderboard (lb/wordle-daily-leaderboard-data)))

(defn chess-leaderboard []
  (draw-leaderboard (lb/chess-leaderboard-data)))

(defn main [_]
  (->>
   (str/join break
             ["# Leaderboards"
              "## Wordle"
              "Score is based on number of guesses taken. For example, guessing on the first try = 6 points, on the second = 5, and so on."
              "### Daily Scores"
              (str "```\n" (wordle-daily-leaderboard) "\n```")
              "### All-time Scores"
              (str "```\n" (wordle-leaderboard) "\n```")
              "## Chess"
              "2 points for a win, 1 point for a tie."
              (str "```\n" (chess-leaderboard) "\n```")
              "=> / Back"])
   (r/success-response r/gemtext)))
