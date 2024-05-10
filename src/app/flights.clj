(require '[space-age.flights :as f])
(require '[space-age.responses :as r])
(require '[clojure.string :as str])

(def root "/src/app/flights")
;; TODO add options to query cities/airports

;; TODO proper parsing and validation
(defn parse-query [q]
  (->>
   (str/split q #" and ")
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

     (str
      "# Flight Info\n\n"
      (str "=> " root " Back\n\n")
      "```"
      msg
      "\n```")
     (r/success-response r/gemtext))))

;; TODO add details
(defn splash-page []
  (->>
   (str "# Flight Info\n\n"
        (str "=> " root "/query Query"))
   (r/success-response r/gemtext)))

(defn main [req]
  (let [path (or (first (:path-args req)) "/")]
    (case path
      "/"     (splash-page)
      "query" (flight-data req)
      "info"  (flight-info-page req)
      "Nothing here")))
