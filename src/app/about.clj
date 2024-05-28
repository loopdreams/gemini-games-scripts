(require '[space-age.responses :as r])

(defn main [_]
  (let [body (slurp "static/partials/site_info")]
    (->> body (r/success-response r/gemtext))))
