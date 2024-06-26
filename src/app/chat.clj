(require '[space-age.db :as db])
(require '[space-age.responses :as r])
(require '[space-age.user-registration :as reg])
(require '[clojure.string :as str])
(import java.text.SimpleDateFormat)
(import java.util.Date)

(def messages-per-page 10)
(def root "/src/app/chat")

(defn write-message [req]
  (if (:query req)
    (let [message {:username (db/get-username req)
                   :message (:query req)
                   :time (.format (SimpleDateFormat. "YYYY-MM-dd HH:mm:ss") (Date.))}]
      (db/chat-insert-message! message)
      {:status 30 :meta root})
    {:status 10 :meta "Enter message"}))

(defn chat-history [page-no]
  (let [messages       (->> (db/chat-get-messages)
                            (sort-by :messages/time)
                            reverse)

        total-messages (count messages)

        messages       (take messages-per-page
                             (if-not (= 1 page-no)
                               (drop (* messages-per-page (dec page-no))
                                     messages)
                               messages))]
    (str
     (str/join "\n\n"
               (for [msg  messages
                     :let [user (:messages/username msg)
                           message (:messages/message msg)
                           time (:messages/time msg)]]
                 (str message "\n"
                      "- " user "@" time)))
     
     (when (and (> total-messages messages-per-page)
                (<= page-no (int (/ total-messages messages-per-page))))
       (str "\n\n=> " root "/page/" (inc page-no) " Next Page"))

     (when (not= page-no 1)
       (str "\n\n=> " root "/page/" (dec page-no) " Previous Page")))))


(defn path-link [name label]
  (str "=> " root "/" name " " label))

(defn chat-main-page [req page-no]
  (let [user (db/get-username req)]
    (->>
     (str
      "# Chat"
      "\n\n=> / Home"
      "\n\n"
      (if-not user
        (path-link "name" "Enter your name")
        (str "Signed is as " user "\n\n"
             (path-link "message" "Write a message")))
      "\n\n"
      "---------------------------------------------\n"
      (chat-history page-no))
     (r/success-response r/gemtext))))
        
(defn main [req]
  (if-not (:client-cert req) (reg/register-user)
          (let [route (or (first (:path-args req)) "/")]
            (case route
              "/"       (chat-main-page req 1)
              "page"    (chat-main-page req (parse-long (second (:path-args req))))
              "name"    (reg/register-name req root)
              "message" (write-message req)
              (r/success-response r/gemtext "Nothing here")))))
