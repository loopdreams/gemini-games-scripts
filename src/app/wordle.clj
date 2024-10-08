(require '[space-age.db :as db])
(require '[space-age.responses :as r])
(require '[space-age.user-registration :as reg])
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[java-time.api :as jt])

(def root "/src/app/wordle")
(def instructions (slurp "static/partials/instructions_wordle"))
(def break "\n\n")

(def guess-limit 6)
(defonce word-of-the-day (db/get-todays-word))
(def default-keyboard-letters "qwertyuiop asdfghjkl  zxcvbnm")

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


(defn time-until-next-word []
  (let [[hrs mins]     (map parse-long
                            (-> (jt/format "HH mm" (jt/local-date-time))
                                (str/split #" ")))
        mins-remaining (- 60 mins)
        hrs-remaining  (- 24 (inc hrs))]
    (cond
      (= mins-remaining 0) (str hrs-remaining " hours until next word.")
      (= hrs-remaining 0) (str mins-remaining " minutes until next word.")
      :else (str hrs-remaining
                 (if (= hrs-remaining 1) " hour" " hours")
                 " and "
                 mins-remaining
                 (if (= mins-remaining 1) " minute" " minutes")
                 " until next word."))))

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

(defn validate-word [guess]
  (@db/valid-words (str/lower-case guess)))

(def correct-letter-position "●")
(def correct-letter "○")
(def incorrect-letter "-")


(defn winner? [guesses]
  (when (seq guesses)
    (->>
     (last (str/split guesses #":"))
     (every? #{(first (seq correct-letter-position))}))))

(defn remove-one [lst to-remove]
  (let [[x y] (split-with (partial not= to-remove) lst)]
    (concat x (rest y))))

(defn calc-matches [word guess]
  (let [wd (seq word)
        gs (seq (str/lower-case guess))]
    (loop [letters     (map-indexed vector gs)
           result      []
           take-ltrs   wd
           cor-pos-idx []
           pass        0]
      (if (zero? pass)
        ;; First pass checking for letters in correct position
        (if (empty? letters)
          (recur (map-indexed vector gs) result take-ltrs cor-pos-idx (inc pass))
          (let [[idx ltr] (first letters)]
            (if (= ltr (nth wd idx))
              (recur (rest letters)
                     (conj result correct-letter-position)
                     (remove-one take-ltrs ltr)
                     (conj cor-pos-idx idx)
                     pass)
              (recur (rest letters)
                     (conj result incorrect-letter)
                     take-ltrs
                     cor-pos-idx
                     pass))))
        ;; Second pass checking for letters that are correct but in the wrong position
        (if (empty? letters)
          result
          (let [[idx ltr] (first letters)]
            (if (and (some #{ltr} wd)
                     (some #{ltr} take-ltrs)
                     (not (some #{idx} cor-pos-idx)))
              (recur (rest letters)
                     (assoc result idx correct-letter)
                     (remove-one take-ltrs ltr)
                     cor-pos-idx
                     pass)
              (recur (rest letters) result take-ltrs cor-pos-idx pass))))))))

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

        current-keyboard (or (db/get-keyboard req) default-keyboard-letters)
        new-keyboard     (->> (incorrect-letters word input)
                              (drop-letters current-keyboard))

        win-condition    (winner? new-guesses)]
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
               (str/join "\n" (interleave board (repeat frames)))
               "\nKey:"
               (str correct-letter-position " Letter is correct and in the right position")
               (str correct-letter " Word contains this letter, but it is in the wrong position")
               (str incorrect-letter " Word does not contain this letter")])))


;; Page
(defn wordle-game-page [req]
  (let [user           (db/get-username req)
        board          (-> (db/get-guesses req)
                           make-board
                           draw-board)
        keyboard-state (-> (or (db/get-keyboard req) default-keyboard-letters)
                           keyboard)
        guess-count    (- (inc guess-limit) (or (db/get-score req) 0))
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
      "``` wordle game board\n"
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
