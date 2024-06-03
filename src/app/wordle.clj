(require '[space-age.db :as db])
(require '[space-age.responses :as r])
(require '[space-age.user-registration :as reg])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[java-time.api :as jt])

(def root "/src/app/wordle")
(def guess-limit 6)
(def break "\n\n")
(defonce word-of-the-day (db/get-todays-word))

(def default-letters "qwertyuiop asdfghjkl  zxcvbnm")

;; Keyboard logic
(defn drop-letter [keyb drop]
  (if (re-find (re-pattern drop) keyb)
    (let [pos (.indexOf keyb drop)]
      (str (subs keyb 0 pos) "_" (subs keyb (inc pos))))
    keyb))


(defn drop-letters [keyb letters]
  (reduce #(drop-letter %1 %2)
          keyb
          letters))

(defn keyboard [letters]
  (->> (partition-all 10 (seq letters))
       (map #(str/join " " %))
       (str/join "\n")))

(def instructions
  (slurp "static/partials/instructions_wordle"))

;; Helper Functions

(defn time-until-next-word []
  (let [[hrs mins :as x] (map parse-long
                              (-> (jt/format "HH mm" (jt/local-date-time))
                                  (str/split #" ")))]
    (if (> mins 0)
      (let [mins-remaining (- 60 mins)
            hrs-remaining (- 24 (inc hrs))]
        (str "- " hrs-remaining ":" mins-remaining " until next word."))
      (str "- " (- 24 hrs) " until next word."))))


(defn path-link [name label]
  (str "=> " root "/" name " " label))

(defn validate-guess [guess]
  (let [valid-range (into #{} (range 97 123))]
    (->> (str/lower-case guess)
         seq
         (filter (comp valid-range int))
         count
         (= 5))))

(defn validate-word [guess]
  (@db/valid-words (str/lower-case guess)))

(defn winner? [guesses]
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

(defn incorrect-letters [word guess]
  (mapv str (set/difference (set guess) (set word))))


;; Guesses stored as a string in 'guesses' column. Guess and markers separated by ':' and
;; individual gusses seperated by ' ' e.g., "apple:xo--x pears:xo--x river:xxxxx"
(defn store-guess [req input]
  (let [word             word-of-the-day
        guesses-state    (db/get-guesses req)
        guess-markers    (->> (calc-matches word input)
                              (apply str))
        new-guesses      (str (when guesses-state (str guesses-state " "))
                              input ":" guess-markers)

        current-keyboard (or (db/get-keyboard req) default-letters)
        new-keyboard     (->> (incorrect-letters word input)
                              (drop-letters current-keyboard))

        win-condition    (winner? new-guesses)]
    ;; TODO find better place to init keyboard
    (db/insert-guess! req new-guesses)
    (db/update-keyboard! req new-keyboard)
    (when win-condition
      (db/update-win-condition! req))))


;; Guess handler
(defn make-guess [req]
  (let [guess (:query req)]
    (if guess
      (if-not (validate-guess guess)
        {:status 10 :meta "Incorrect input, please enter 5 letters only"}

        (if-not (validate-word guess)
          {:status 10 :meta (str guess " is not a valid word, please try again.")}

          (do (store-guess req (:query req))
              {:status 30 :meta root})))
        
      {:status 10 :meta "Enter guess"})))

;; Game Board
(defn format-row [r]
  (str " " (str/join " " (interleave r (repeat " ")))))

(defn unpack-row-data [row-data]
  (->> (str/split row-data #" ")
       (map #(str/split % #":"))))

(defn make-board [row-data]
  (if-not row-data ["                     "]
          (let [rows (unpack-row-data row-data)]
            (for [r rows
                  :let [[guess markers] r]]
              (str (format-row guess)
                   "\n"
                   (format-row markers))))))

(defn draw-board [board]
  (let [frames "-------------------"]
    (str/join "\n"
              [frames
               (str/join "\n" (interleave board (repeat frames)))])))


;; Page
(defn wordle-game-page [req]
  (let [user           (db/get-username req)
        board          (-> (db/get-guesses req)
                           make-board
                           draw-board)
        keyboard-state (-> (or (db/get-keyboard req) default-letters)
                           keyboard)
        guess-count    (- (inc guess-limit) (db/get-score req))
        win-condition  (db/win-condition req)
        daily-word     word-of-the-day]
    (->>
     (str
      "# Wordle"
      break
      "=> / Home"
      break
      "A gemini clone of Wordle. Try to guess the five letter word."
      break
      (if-not user
        (path-link "name" "Enter your name")

        (cond
          (= win-condition 1)
          (str "You won " user "!\nIt took you " guess-count " guesses."
               break
               (reg/wordle-stats req))

          (= guess-limit guess-count)
          (str "Out of guesses! The word was: " daily-word)

          :else (path-link "guess" "Make a guess")))

      break
      "```\n"
      keyboard-state
      break
      board
      "\n```"
      break
      instructions
      break
      (time-until-next-word))

     (r/success-response r/gemtext))))

;; Routes
(defn main [req]
  (if-not (:client-cert req)
    (reg/register-user)

    (let [route (or (first (:path-args req)) "/")]
      (case route
        "/"       (wordle-game-page req)
        "name"    (reg/register-name req root)
        "guess"   (make-guess req)
        (r/success-response r/gemtext "Nothing here")))))
