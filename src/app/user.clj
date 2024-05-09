(require '[space-age.db :as db])
(require '[space-age.responses :as r])
(require '[space-age.user-registration :as reg])

(def root "/src/app/user")
(def break "\n\n")

(defn user-stats [req]
  (str "## Your Stats"
       break
       "### Wordle\n"
       (reg/wordle-stats req)
       break
       "### Chess\n"
       (reg/chess-history req)))
  

(defn user-page [req]
  (let [user (db/get-username req)]
    (->>
     (str "# Userpage \n\n"
          "=> / Home \n\n"

          (if-not user
            (str "=> " root "/name " "Enter your name")
            (str "Hello " user "!\n\n"
                 "=> " root "/update-name " "Change your name."))
          break
          (user-stats req))

     (r/success-response r/gemtext))))

(defn main [req]
  (if-not (:client-cert req)
    (reg/register-user)

    (let [route (or (first (:path-args req)) "/")]
      (case route
        "/"           (user-page req)
        "name"        (reg/register-name req root)
        "update-name" (reg/update-name req root)
        (r/success-response r/gemtext "Nothing here")))))
