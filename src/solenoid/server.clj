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
            [solenoid.utils :as u])
    (:import (java.net ServerSocket BindException)))

(defn- app-body
  []
  [:section#app
   (into
     [:div.grid.sm:grid-cols-2.md:grid-cols-3.lg:grid-cols-4.xl:grid-cols-5.grid-flow-row.gap-4.m-4]
     (concat
         ;; render any controllers not associated with control blocks (can do this by using c/cursor directly)
         (mapv components/render-controller
               (remove #(or (not (map? % ))
                            (contains? % :block-id)
                            (str/includes? (name (:id %)) "controlblock")
                            (str/includes? (name (:id %)) "viewblock"))
                       (vals @c/registry)))
         ;; render the control blocks in the registry
         (->> @c/registry
              vals
              (filter #(and (map? %)
                            (or (str/includes? (name (:id %)) "controlblock")
                                (str/includes? (name (:id %)) "viewblock"))))
              (sort-by #(-> % :id str (str/split #"-") second parse-long))
              (mapv components/render-control-block))))])

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
                #_[:style (slurp (io/resource "bootstrap.min.css"))]
                #_[:script (slurp (io/resource "bootstrap.bundle.min.js"))]
                #_[:style (slurp (io/resource "style.css"))]
                [:style (slurp (io/resource "output.css"))]
                [:script (slurp (io/resource "htmx.min.js"))]
                [:script (slurp (io/resource "sse.js"))]]
               head-entries))
       [:body.dark:bg-slate-300
        [:span {:class       "container-fluid"
                :hx-ext      "sse"
                :sse-connect "/events"}
         [:div#reloader
          {:hx-trigger "sse:reload"
           :hx-get     "/reload"
           :hx-target  "#reloader"}
          (app-body)]]
        [:footer
         [:h3 "solenoid"]
         [:div
          [:p "Created by "
           [:a {:href "https://twitter.com/RustyVermeer"} "adam-james"]]]]]))))

(defn- get-dependents
  [block-id]
  (->> (get-in @c/registry [block-id :state])
       u/get-watches
       (map last)
       (remove nil?)
       (filter #(str/starts-with? (name %) "controlblock"))))

(defn- get-nested-dependents
  [block-id]
  (let [ifn   (fn [ids] (mapcat get-dependents ids))
        iters (take 20 (take-while seq (iterate ifn [block-id])))]
    (->> iters
         (apply concat)
         distinct)))

;; PROBLEM: Clean up the routes by pulling fns out and using defn -> good for documentation/readability
(defn- routes
  ([] (routes nil))
  ([opts]
   [{:path     "/"
     :method   :get
     :response (fn [_]
                 {:status 200
                  :body   (let [out (html (template opts))]
                            (spit "sample.html" out)
                            out)})}
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
                       dependents                            (remove #{block-id} (get-nested-dependents block-id))]
                   (when (not (nil? value)) (swap! c/registry assoc-in [id :value] value))
                   {:status 200
                    :body   (apply str
                                   (concat
                                     [ ;; render a controller
                                      (html (case (keyword control-type)
                                              :point (components/render-point-value {:value value})
                                              (str value)))
                                      ;; when rendering a block, they're updated with hx out-of-band true
                                      (when block-id
                                        (html (components/render-control-block-result
                                                (get @c/registry block-id)
                                                true)))]
                                     ;; when there are dependent control blocks, send their results too
                                     (when (seq dependents)
                                       (for [block-id dependents]
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

;; https://github.com/prestancedesign/get-port/blob/main/src/prestancedesign/get_port.clj
(defn- get-available-port
  "Return a random available TCP port in allowed range (between 1024 and 65535) or a specified one"
  ([] (get-available-port 0))
  ([port]
   (with-open [socket (ServerSocket. port)]
     (.getLocalPort socket))))

(defn get-port
  "Get an available TCP port according to the supplied options.
  - A preferred port: (get-port {:port 3000})
  - A vector of preferred ports: (get-port {:port [3000 3004 3010]})
  - Use the `make-range` helper in case you need a port in a certain (inclusive) range: (get-port {:port (make-range 3000 3005)})
  No args return a random available port"
  ([] (get-available-port))
  ([opts]
   (loop [port (:port opts)]
     (let [result
           (try
             (get-available-port (if (number? port) port (first port)))
             (catch Exception e (instance? BindException (.getCause e))))]
       (or result (recur (if (number? port) 0 (next port))))))))

(defn serve!
  [& {:keys [port] :as opts}]
  (stop-server)
  (let [available-port (get-port {:port (concat [(or port 9876)] (range 8000 9000))})]
    (reset! server (srv/run-server (#'app opts) {:port available-port}))
    (println "Server started on Port: " available-port)))
