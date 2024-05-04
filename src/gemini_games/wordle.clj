(require '[space-age.db :as db])
(require '[space-age.responses :as r])
(require '[space-age.user-registration :as reg])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def root "/src/gemini_games/wordle")
(def guess-limit 6)
(def break "\n\n")


(def instructions
  (str/join break
            ["## Instructions"
             "The aim of the game is to guess a five letter word is as little tries as possible."
             "After each guess, there will be indicators for each letter showing whether the letter was correct and in the right position ('x'), whether the letter was right but in the wrong position ('o') or whether the letter was incorrect ('-')."
             "Unlike other versions of this game, this verion will not check whether the word is a valid word. For example, the guess 'abdce' will be accepted. However, only valid words are included in the word bank."
             "There will be a new word every day."]))

(def word "apple")

;; Helper Functions

(defn path-link [name label]
  (str "=> " root "/" name " " label))

(defn validate-guess [guess]
  (let [valid-range (into #{} (range 97 123))]
    (->> (str/lower-case guess)
         seq
         (filter (comp valid-range int))
         count
         (= 5))))

(defn win? [guesses]
  (when (seq guesses)
    (->>
     (last (str/split guesses #":"))
     (every? #{\x}))))

(defn calc-matches [word guess]
  (let [wd            (seq word)
        gs            (seq (str/lower-case guess))
        intersections (set/intersection (set wd) (set gs))
        positions     (mapv #(if (= %1 %2) "x" "-") wd gs)
        checked       (->> (partition 2 (interleave wd gs))
                           (filter (fn [[a b]] (= a b)))
                           (map first)
                           (into #{}))]
    (loop [[g & gss] gs
           result    positions
           count     0]
      (if-not g
        result
        (if (and (intersections g) (not (checked g)))
          (recur gss (assoc result count "o") (inc count))
          (recur gss result (inc count)))))))



;; Guesses stored as a string in 'guesses' column. Guess and markers separated by ':' and
;; individual gusses seperated by ' ' e.g., "apple:xo--x pears:xo--x river:xxxxx"
(defn store-guess [req input]
  (let [word          word
        guesses-state (db/get-guesses req)
        guess-markers (->> (calc-matches word input)
                           (apply str))
        new-guesses   (str (when guesses-state (str guesses-state " "))
                           input ":" guess-markers)
        win-condition (win? new-guesses)]
    (db/insert-guess! req new-guesses)
    (when win-condition
      (db/update-win-condition! req))))

;; Guess handler
(defn make-guess [req]
  (let [guess (:query req)]
    (if guess
      (if-not (validate-guess guess)
        {:status 10 :meta "Incorrect input, please enter 5 letters only"}

        (do (store-guess req (:query req))
            {:status 30 :meta root}))
        
      {:status 10 :meta "Enter guess"})))

;; Game Board
(defn format-row [r]
  (str "| " (str/join " " (interleave r (repeat "|")))))

(defn unpack-row-data [row-data]
  (->> (str/split row-data #" ")
       (map #(str/split % #":"))))

(defn make-board [row-data]
  (if-not row-data ["|   |   |   |   |   |"]
          (let [rows (unpack-row-data row-data)]
            (for [r rows
                  :let [[guess markers] r]]
              (str (format-row guess)
                   "\n"
                   (format-row markers))))))

(defn draw-board [board]
  (let [frames "---------------------"]
    (str/join "\n"
              ["```"
               frames
               (str/join "\n" (interleave board (repeat frames)))
               "```"])))

(def test-scores [{:games/score 3 :games/win 1}
                  {:games/score 6 :games/win 0}
                  {:games/score 2 :games/win 1}
                  {:games/score 3 :games/win 1}
                  {:games/score 4 :games/win 1}
                  {:games/score 4 :games/win 1}
                  {:games/score 5 :games/win 1}
                  {:games/score 3 :games/win 1}
                  {:games/score 3 :games/win 1}
                  {:games/score 3 :games/win 1}
                  {:games/score 6 :games/win 0}
                  {:games/score 3 :games/win 1}
                  {:games/score 3 :games/win 1}])
;; User stats

(def bar-symbol (char 9632))

(defn bar-string [percentage count]
  (let [len (* 20 (/ percentage 100))]
    (str "[" (str/join (repeat len bar-symbol)) "] " count)))

(defn stats-bars [win-frequencies]
  (let [[[_ full]] win-frequencies]
    (for [i (range 1 (inc guess-limit))
          :let [[_ len] (or (first (filter #(= (first %) i) win-frequencies))
                            [i 0])
                percentage (* 100 (/ len full))]]
      (bar-string percentage len))))


(defn user-stats [stats]
  (let [total-games (count stats)
        wins        (filter #(= (:games/win %) 1) stats)
        win-count   (count wins)
        win-rate    (int (* 100 (/ win-count total-games)))
        scores      (->> (map :games/score wins)
                         frequencies
                         (sort-by second)
                         reverse
                         stats-bars
                         (str/join "\n"))]
                        
    (str "Total games played: " total-games "\n"
         "Win rate: " win-rate "%\n"
         "```\n"
         "---------------------\n"
         scores
         "\n---------------------"
         "\n```")))


;; Page
(defn wordle-page [req]
  (let [user          (db/get-username req)
        board         (-> (db/get-guesses req)
                          make-board
                          draw-board)
        guess-count   (db/get-score req)
        win-condition (db/win-condition req)
        daily-word    word]
    (->>
     (str
      "# Wordle"
      break
      "=> / Home"
      break
      "This is a gemini clone of the well-known game Wordle. Some brief instructions are below."
      break

      (if-not user
        (path-link "name" "Enter your name")

        (str "Logged in as " user
             break

             (cond
               (= win-condition 1)
               (str "You won " user "!\nIt took you " guess-count " guesses."
                    break
                    (user-stats test-scores))

               (= guess-limit guess-count)
               (str "Out of guesses! The word was " daily-word)

               :else (path-link "guess" "Make a guess"))))

      break
      board
      break
      instructions)

     (r/success-response r/gemtext))))

;; Routes
(defn main [req]
  (if-not (:client-cert req)
    (reg/register-user)

    (let [route (or (first (:path-args req)) "/")]
      (case route
        "/"       (wordle-page req)
        "name"    (reg/register-name req root)
        "guess"   (make-guess req)
        (r/success-response r/gemtext "Nothing here")))))
