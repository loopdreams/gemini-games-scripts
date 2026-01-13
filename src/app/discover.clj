(require '[space-age.responses :as r])
(require '[space-age.logging :as log])
(import '[javax.net.ssl SSLContext X509TrustManager])
(import '[java.io BufferedReader InputStreamReader OutputStreamWriter])
(import '[java.security MessageDigest SecureRandom])
(import '[java.util Date])
(import '[java.time Instant ZoneId LocalDate Period])

(def capsules-list-store "data/capsules_list.edn")
(def root "/src/app/discover")

(defn load-store [] (clojure.edn/read-string (slurp capsules-list-store)))

(defn sha256-fingerprint [^java.security.cert.X509Certificate cert]
  (let [md (MessageDigest/getInstance "SHA-256")
        der (.getEncoded cert)
        hash (.digest md der)]
    (apply str (map #(format "%02x" (bit-and % 0xff)) hash))))

(def trust-manager
  (proxy [X509TrustManager] []
    (getAcceptedIssuers [] nil)
    (checkClientTrusted [_ _])
    (checkServerTrusted [chain _]
      (let [cert (first chain)
            fp (sha256-fingerprint cert)]
        (when (not= fp (:lupa-cert (load-store)))
          (log/log "[WARN] discover.clj Certificate fingerprint mismatch on lupa connect. Capsule list not updated...")
          (throw (SecurityException.)))))))

(defn ssl-socket-factory []
  (.getSocketFactory
   (doto (SSLContext/getInstance "TLS")
     (.init nil (into-array [trust-manager]) (SecureRandom.)))))

(defn capsule-links-request
  "A very basic client request for a single resource."
  []
  (let [factory (ssl-socket-factory)
        socket (.createSocket factory "gemini.bortzmeyer.org" 1965)]
    (doto (OutputStreamWriter. (.getOutputStream socket))
      (.write "gemini://gemini.bortzmeyer.org/software/lupa/lupa-capsules.txt\r\n")
      (.flush))
    (line-seq
     (BufferedReader.
      (InputStreamReader. (.getInputStream socket))))))

(defn save-new-capsules-list! [list]
  (let [{:keys [lupa-cert]} (load-store)
        date                (Date.)
        names               (mapv #(str "gemini://" %) list)]
    (spit capsules-list-store
          {:lupa-cert lupa-cert
           :date      date
           :capsules  names})))

(defn get-and-save-capsule-links! []
  (let [[status & links] (capsule-links-request)
        status-code (subs status 0 2)]
    (if (= status-code "20")
      (do (save-new-capsules-list! links)
          (log/log "[INFO] discover.clj Capsule list updated"))
      (log/log "[WARN] discover.clj Error updating capsule list" status-code))))

(defn time->zoned [time]
  (.toLocalDate (.atZone time (ZoneId/systemDefault))))

(defn more-than-one-month-old? [date]
  (let [date (time->zoned (.toInstant date))
        cur (time->zoned (Instant/now))]
    (.isAfter cur (.plus date (Period/ofMonths 1)))))

(defn get-random-capsule-link []
  (let [{:keys [capsules date]} (load-store)]
    (when (more-than-one-month-old? date)
      (future
        (get-and-save-capsule-links!)))
    (rand-nth capsules)))

(defn random-capsule-link-redirect []
  (-> (get-random-capsule-link)
      (r/permanent-redirect-response)))

(defn main-page []
  (->>
   (str "# Discover Gemini\n\n"
        "=> " root "/random Take me someplace interesting\n\n"
        "This link above takes you to a random gemini capsule. It uses the list of known gemini urls from the wonderful lupa.\n"
        "=> gemini://gemini.bortzmeyer.org/software/lupa/ Lupa")
   (r/success-response r/gemtext)))

(defn main [req]
  (let [route (or (first (:path-args req)) "/")]
    (case route
      "/" (main-page)
      "random" (random-capsule-link-redirect))))
   

