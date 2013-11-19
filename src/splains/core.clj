(ns splains.core
  (:gen-class))

(defn layout
  [content]
  [:html
   [:head
    [:link {:rel "stylesheet" :href "/bootstrap/css/bootstrap.min.css"}]
    [:link {:rel "stylesheet" :href "/css/style.css"}]]
   [:body
    [:div.container-fluid
     [:div.row-fluid
      [:a.btn {:href "/"} "Quotes"]
      [:a.btn {:href "/add-quote"} "Add quote"]]
     [:div.row-fluid content]]
    [:script {:src "/js/debug.js"}]]])

(defn home
  [{:keys [search]}]
  (layout ""))

(defroutes routes
  (GET "/" {params :params} (home params))
  (GET "/add-quote" [] (add-quote-form-page))
  (POST "/add-quote" {params :params} (process-quote-form params)))

(def handler
  (-> #'routes
      site
      (wrap-resource "public")
      (wrap-file-info)))

(defonce server-process (atom nil))

(defn stop-server!
  []
  (when-let [s @server-process]
    (.stop s)
    (reset! server-process nil)))

(defn start-server!
  []
  (stop-server!)
  (reset! server-process (serve handler {:port 3000 :open-browser? false :join? false})))

(defn -main
  [& args]
  (start-server!))
