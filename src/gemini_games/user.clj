(require '[space-age.db :as db])
(require '[space-age.responses :as r])
(require '[space-age.user-registration :as reg])

(def root "/src/gemini_games/user")

(defn user-page [req]
  (let [user (db/get-username req)]
    (->>
     (str "# Userpage \n\n"
          "=> / Home \n\n"

          (if-not user
            (str "=> " root "/name " "Enter your name")
            (str "Hello " user "!\n\n"
                 "=> " root "/update-name " "Change your name.")))

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
