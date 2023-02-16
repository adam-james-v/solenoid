(ns solenoid.server
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [hiccup.core :as hiccup :refer [html]]
            [org.httpkit.server :as srv]
            [ring-sse-middleware.core :as sse-r]
            [ring-sse-middleware.wrapper :as sse-w]
            [ring-sse-middleware.adapter.http-kit :as sse-h]
            [ruuter.core :as ruuter]
            [solenoid.controls :as c]
            [solenoid.components :as components]
            [solenoid.utils :as u]))

#_(defmacro ^:private html
  [options & content]
  `(str (hiccup/html {:escape-strings? false} ~options ~@content)))

(defn- app-body
  []
  [:section#app
   [:main
    [:div.container
     (into
       [:div.row.row-cols-1.row-cols-sm-1.row-cols-md-2.row-cols-lg-3.row-cols-xl-4.g-2]
       (concat
         ;; render any controllers not associated with control blocks (can do this by using c/cursor directly)
         (mapv components/render-controller
               (remove #(or (contains? % :block-id)
                            (str/includes? (name (:id %)) "controlblock"))
                       (vals @c/registry)))
         ;; render the control blocks in the registry
         (mapv components/render-control-block
               (filter #(str/includes? (name (:id %)) "controlblock")
                       (vals @c/registry)))))]]])

(defn- template
  ([] (template nil))
  ([{:keys [head-entries]}]
   (list
     "<!DOCTYPE html>"
     (html
       (into [:head]
             (concat
               [[:meta {:charset "UTF-8"}]
                [:meta {:name    "viewport"
                        :content "width=device-width, initial-scale=1"}]
                [:title "solenoid"]
                [:style (slurp (io/resource "bootstrap.min.css"))]
                [:script (slurp (io/resource "bootstrap.bundle.min.js"))]
                [:style (slurp (io/resource "style.css"))]
                [:script (slurp (io/resource "htmx.min.js"))]
                [:script (slurp (io/resource "sse.js"))]]
               head-entries))
       [:body.d-flex.flex-column.h-100
        {:data-bs-theme "dark"}
        [:span {:class       "container-fluid"
                :hx-ext      "sse"
                :sse-connect "/events"}
         [:div#reloader.row
          {:hx-trigger "sse:reload"
           :hx-get     "/reload"
           :hx-target  "#reloader"}
          (app-body)]]
        [:footer.footer.mt-auto.py-3
         [:h3.mt-4.text-center "solenoid"]
         [:div.container
          [:p "Created by "
           [:a {:href "https://twitter.com/RustyVermeer"} "adam-james"]]]]]))))

;; PROBLEM: Clean up the routes by pulling fns out and using defn -> good for documentation/readability
(defn- routes
  ([] (routes nil))
  ([opts]
   [{:path     "/"
     :method   :get
     :response (fn [_]
                 {:status 200
                  :body   (html (template opts))})}
    {:path   "/action/:action/:id"
     :method :post
     :response
     ;; PROBLEM: This could be reworked with defmethod to allow users to define custom actions
     (fn [{:keys [params]}]
       (let [{:keys [id action]} params
             id                  (u/stringified-key->keyword id)
             action              (u/stringified-key->keyword action)
             control-ids         (get-in @c/registry [id :control-ids])]
         (case action
           :delete
           ;; dissoc all of the controls and the control-block in one go, causing only one :reload
           (swap! c/registry (fn [m] (apply (partial dissoc m) (conj control-ids id))))

           :def
           (let [state (get-in @c/registry [id :state])
                 value (if state
                         @state
                         (get-in @c/registry [id :value]))]
             (intern 'user (symbol (name id)) value))

           ;; no action
           (println "no action"))
         {:status 200
          :body   nil}))}
    ;; hit when a control is altered in the UI. ID and value must always be available
    {:path     "/controller/:id"
     :method   :get
     :response (fn [{:keys [params query-string]}]
                 (let [{:keys [id]}                          params
                       id                                    (u/stringified-key->keyword id)
                       {:keys [value block-id control-type]} (u/query-string->map query-string)
                       value                                 (if (= :text (keyword control-type))
                                                               (u/get-string-value query-string)
                                                               value)
                       block-id                              (when block-id (u/stringified-key->keyword block-id))
                       dependents                            (->> (get-in @c/registry [block-id :state]) u/get-watches)]
                   (when value (swap! c/registry assoc-in [id :value] value))
                   {:status 200
                    :body   (apply str
                                   (concat
                                     [;; render a controller
                                      (html (if (= (keyword control-type) :edn) (str value) value))
                                      ;; when rendering a block, they're updated with hx out-of-band true
                                      (when block-id
                                        (html (components/render-control-block-result
                                                (get @c/registry block-id)
                                                true)))]
                                     ;; when there are dependent control blocks, send their results too
                                     (when (seq dependents)
                                       (for [[_ block-id] dependents]
                                         (html (components/render-control-block-result
                                                 (get @c/registry block-id)
                                                 true))))))}))}
    ;; gets hit when you (reset! event :reload) on the backend
    {:path     "/reload"
     :method   :get
     :response (fn [_req]
                 {:status 200
                  :body   (html (app-body))})}]))

;; PROBLEM: this works perfectly EXCEPT it will only refresh the browser if its the last focused window?
;; PROBLEM: should use 'idiomorph' for merge swapping, which should help preserve focus and state better
(defn- app
  ([] (app nil))
  ([opts]
   (-> #(ruuter/route (routes opts) %)
       (sse-r/streaming-middleware
         sse-h/generate-stream
         {:request-matcher (fn [req] ((partial sse-r/uri-match "/events") req))
          :chunk-generator (-> (fn [_]
                                 (let [evt @c/event]
                                   (if (= evt :reload)
                                     (do (reset! c/event :waiting)
                                         "event: reload\ndata:\"\"")
                                     "waiting...")))
                               (sse-w/wrap-delay 100)
                               sse-w/wrap-sse-event
                               sse-w/wrap-pst)}))))

(defonce ^:private server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn serve!
  [& {:keys [port] :as opts}]
  (reset! server (srv/run-server (#'app opts) {:port (or port 9876)})))
