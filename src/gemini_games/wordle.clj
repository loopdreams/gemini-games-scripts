(require '[space-age.db :as db])
(require '[space-age.responses :as r])
(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def root "/src/gemini_games/wordle")
(def guess-limit 6)

(def word "apple")

;; Helper Functions
(defn client-id [req]
  (-> req
      :client-cert
      :sha256-hash))

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
(defn store-guess [username input]
  (let [word          word
        guesses-state (db/get-guesses username)
        guess-markers (->> (calc-matches word input)
                           (apply str))
        new-guesses   (str (when guesses-state (str guesses-state " "))
                           input ":" guess-markers)
        win-condition (win? new-guesses)]
    (db/insert-guess! username new-guesses)
    (when win-condition
      (db/update-win-condition! username))))

;; Guess handler
(defn make-guess [req]
  (let [guess (:query req)]
    (if guess
      (if-not (validate-guess guess)
        {:status 10 :meta "Incorrect input, please enter 5 letters only"}

        (do (store-guess (db/get-username (client-id req))
                         (:query req))
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


(defn homepage [req]
  (let [user (db/get-username (client-id req))
        board (-> (db/get-guesses user)
                  make-board
                  draw-board)
        guess-count (db/get-score user)
        win-condition (db/win-condition user)
        daily-word word]
    (println (class win-condition))
    (->>

     (str
      "# Wordle \n\n"

      (if-not user

        (path-link "name" "Enter your name")
        (str "Logged in as " user
             "\n"

             (cond
               (= win-condition 1)
               (str "You won " user "!\nIt took you " guess-count " guesses" "\n TODO Stat Table")

               (= guess-limit guess-count)
               (str "Out of guesses! The word was " daily-word)

               :else (path-link "guess" "Make a guess"))))

      "\n\n"

      board)

     (r/success-response r/gemtext))))


;; Registration
(defn register-user []
  {:status 60
   :meta "Please attach your client certificate"})

(defn register-name [req]
  (if (:query req)
    (do (db/register-user! (client-id req) (:query req))
        {:status 30 :meta root})
    {:status 10 :meta "Enter name"}))


(defn main [req]
  (if-not (:client-cert req) (register-user)
          (let [route (or (first (:path-args req)) "/")]
            (case route
              "/"       (homepage req)
              "name"    (register-name req)
              "guess"   (make-guess req)
              (r/success-response r/gemtext "Nothing here")))))
