(require '[space-age.flights :as f])
(require '[space-age.responses :as r])
(require '[clojure.string :as str])

(def root "/src/app/flights")
(def break "\n\n")

(def instructions-text (slurp "static/partials/instructions_flights"))
(def about-text (slurp "static/partials/about_flights"))


;; TODO add options to query cities/airports

;; TODO proper parsing and validation
(defn parse-query [q]
  (->>
   (str/split q #"\;")
   (map str/trim)))

(defn flight-data [req]
  (if (:query req)
    (let [[from to] (parse-query (:query req))]
      {:status 30 :meta (str root "/info/" from "/" to)})
    {:status 10 :meta "Input query"}))


(defn flight-info-page [req]
  (let [[_ from to] (:path-args req)
        msg (f/flight-message [from to])]
    (->>

     (str/join
      break
      ["# Flight Info"
       (str "=> " root " Flights")
       (str "=> " root "/query New Query")
       "``` Flight info"
       msg
       "```"])
     (r/success-response r/gemtext))))

(defn splash-page []
  (->>
   (str/join
    break
    ["# Flight Info"
     (str "=> " "/ Home")
     (str "=> " root "/query Query")
     instructions-text
     about-text])
   (r/success-response r/gemtext)))

(defn main [req]
  (let [path (or (first (:path-args req)) "/")]
    (case path
      "/"     (splash-page)
      "query" (flight-data req)
      "info"  (flight-info-page req)
      "Nothing here")))
