(ns solenoid.server
  (:require [cheshire.core :as json]
            [clojure.data :as d]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hiccup.core :as hiccup :refer [html]]
            [org.httpkit.server :as srv]
            [ruuter.core :as ruuter]
            [solenoid.controls :as c]
            [solenoid.components :as components]
            [solenoid.utils :as u])
    (:import (java.net ServerSocket BindException)))

(def ^:private extras-init
  "
htmx.onLoad(function(content) {
 var sortables = content.querySelectorAll('.sortable');
 for (var i = 0; i < sortables.length; i++) {
   var sortable = sortables[i];
   new Sortable(sortable, {
       group: 'asdf',
       filter: '.immovable',
       animation: 100,
       handle: '.drag-handle',
       ghostClass: 'blue-background-class'
   });
 }
 var draggables = content.querySelectorAll('.draggable');
 for (var i = 0; i < draggables.length; i++) {
   var draggable = draggables[i];
   makeDraggable(draggable);
 }
})")

(defn get-blocks
  []
  (vec (filter #(or (str/includes? % "controlblock")
                    (str/includes? % "viewblock"))
               (keys @c/registry))))

(defn- app-body
  []
  (let [block-keys (:block-order @c/registry)
        ks         (if (seq block-keys)
                     block-keys
                     (get-blocks))
        blocks     (pmap #(components/render-control-block (get @c/registry %)) ks)]
    [:section#app
     ;; WIP sidebar
     #_[:div.grid.sm:grid-cols-1.grid-flow-row.gap-2.sortable.bg-red-700
        {:style      {:width    "400px"
                      :height   "90vh"
                      :position "absolute"}
         :id         "side-container"
         :hx-trigger "end"
         :hx-get     "/items"
         :hx-swap    "none"
         :hx-include "[name='item']"}]
     (into
       [:div.grid.sm:grid-cols-2.md:grid-cols-12.lg:grid-cols-20.xl:grid-cols-24.grid-flow-row.gap-4.xl:mx-24.m-4.sortable
        {:id         "grid-container"
         :hx-trigger "end"
         :hx-get     "/items"
         :hx-swap    "none"
         :hx-include "[name='item']"}]
       ;; render the control blocks in the registry
       blocks)]))

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
                [:style (slurp (io/resource "style.css"))]
                [:style (slurp (io/resource "output.css"))]
                #_[:script (slurp (io/resource "htmx.min.js"))]
                [:script {:src "https://unpkg.com/htmx.org/dist/htmx.js"}]
                [:script {:src "https://unpkg.com/htmx.org/dist/ext/ws.js"}]
                [:script {:src "https://unpkg.com/morphdom/dist/morphdom-umd.js"}]
                [:script {:src "https://unpkg.com/htmx.org/dist/ext/morphdom-swap.js"}]
                ;; sortable
                [:script {:src "http://SortableJS.github.io/Sortable/Sortable.js"}]
                [:script extras-init]
                ;; draggable SVG elements
                [:script (slurp (io/resource "make-draggable.js"))]]
               head-entries))
       [:body
        {:hx-ext "ws,morphdom-swap"
         :style {#_#_:background        "conic-gradient(from .5turn at top right, darkseagreen, darkslategray)"
                 :background            "conic-gradient(from 90deg at bottom right, cyan, rebeccapurple)"
                 :height                "100%"
                 :background-attachment "fixed !important"}}
        [:div {:ws-connect "/socket"}
         (app-body)]
        #_[:footer
           {:class ["sticky" "top-[90vh]"]}
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

;; websocket handling

(def channels (atom #{}))

(defn connect! [channel]
  (println "Channel Opened")
  (swap! channels conj channel))

(defn disconnect! [channel status]
  (println "Channel Closed: " status)
  (swap! channels disj channel))

(defn handle-message! [_channel ws-message]
  (let [message (json/parse-string ws-message keyword)]
    (println "nothing to do with message: " message)))

(defn broadcast!
  [html-str]
  (let [f (fn [channel]
            (if (srv/open? channel)
              (srv/send! channel html-str)
              (do (srv/close channel)
                  (disconnect! channel :connection-lost))))]
    (doall (pmap f @channels))))

(defn ws-handler [request]
  (println "initial ws request made.")
  (srv/as-channel request
                  {:on-receive handle-message!
                   :on-close   disconnect!
                   :on-open    connect!}))

(defn initial-response
  [opts]
  (fn [_]
    {:status 200
     :body   (let [out (html (template opts))]
               (spit "sample.html" out) ;; this is just for tailwind dev
               out)}))

(defn action-response
  [{:keys [params query-string]}]
  (let [{:keys [id action]} params
        id                  (u/stringified-key->keyword id)
        action              (u/stringified-key->keyword action)
        control-ids         (get-in @c/registry [id :control-ids])]
    (case action
      :delete
      ;; dissoc all of the controls and the control-block in one go, causing only one :reload
      (do
        (swap! c/registry update :block-order (fn [v] (vec (remove #{id} v))))
        (swap! c/registry (fn [m] (apply (partial dissoc m) (conj control-ids id)))))

      :def
      (let [state (get-in @c/registry [id :state])
            value (if state
                    @state
                    (get-in @c/registry [id :value]))]
        (intern 'user (symbol (name id)) value))

      :adjust-size
      (let [{:keys [direction op]} (u/query-string->map query-string)
            [direction op]         (map keyword [direction op])
            op                     (get {:+ inc :- dec} op)
            dim-key                (get {:width :grid-w :height :grid-h} direction)
            current-dim            (get-in @c/registry [id dim-key] 4)]
        (when (< 0 (op current-dim) 20)
          (swap! c/registry assoc-in [id dim-key] (op current-dim)))
        (html (components/render-control-block (get @c/registry id))))

      ;; no action
      (println "no action"))
    {:status 200
     :body   nil}))

(defn controller-response
  [{:keys [params query-string]}]
  (let [{:keys [id]}                          params
        id                                    (u/stringified-key->keyword id)
        {:keys [value block-id control-type]} (u/query-string->map query-string)
        value                                 (if (= :text (keyword control-type))
                                                (u/get-string-value query-string)
                                                value)
        block-id                              (when block-id (u/stringified-key->keyword block-id))
        dependents                            (remove #{block-id} (get-nested-dependents block-id))]
    (when (not (nil? value)) (swap! c/registry assoc-in [id :value] value))
    (let [altered (concat
                    [(components/render-controller (get @c/registry id))
                     (when block-id
                       (components/render-control-block-result (get @c/registry block-id)))]
                    (when (seq dependents)
                      (for [block-id dependents]
                        (components/render-control-block-result (get @c/registry block-id)))))]
      (broadcast! (apply str (map #(html %) altered)))
      {:status 200
       :body   nil})))

(defn items-response
  [{:keys [query-string]}]
  (let [{:keys [item]} (u/query-string->map query-string)
        items          (if (seq item) item [item])]
    (swap! c/registry assoc :block-order (mapv keyword items))
    {:status 200
     :body   nil}))

(defn- routes
  ([] (routes nil))
  ([opts]
   [{:path     "/socket"
     :method   :get
     :response (cookies/wrap-cookies ws-handler {"asdf" {:value "wasd"}})}
    {:path     "/"
     :method   :get
     :response (initial-response opts)}
    {:path     "/action/:action/:id"
     :method   :get
     :response action-response}
    ;; hit when a control is altered in the UI. ID and value must always be available
    {:path     "/controller/:id"
     :method   :get
     :response controller-response}
    ;; sortable ordering
    {:path     "/items"
     :method   :get
     :response items-response}]))

;; PROBLEM: this works perfectly EXCEPT it will only refresh the browser if its the last focused window?
;; PROBLEM: should use 'idiomorph' for merge swapping, which should help preserve focus and state better

(defn- app
  ([] (app nil))
  ([opts]
   #(ruuter/route (routes opts) %)))

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

(defn- block-dim-change?
  [[_ new _]]
  (let [keys-from-maps (->> (vals new)
                            (filter map?)
                            (mapcat keys)
                            distinct)]
    (boolean (seq (filter #{:grid-w :grid-h} keys-from-maps)))))

(defn registry-change-broadcaster
  [_ _ old new]
  ;; only perform actions when controllers are added or removed
  (intern 'user 'd (d/diff old new))
  (when (or
          ;; block-order changes
          (not= (:block-order old) (:block-order new))
          ;; block is added or removed
          (not= (keys old) (keys new))
          ;; block dims change
          (block-dim-change? (d/diff old new)))
    (broadcast! (html (app-body)))))

(add-watch c/registry :registry-change-broadcaster registry-change-broadcaster)

(defn serve!
  [& {:keys [port] :as opts}]
  (stop-server)
  (let [available-port (get-port {:port (concat [(or port 9876)] (range 8000 9000))})]
    (reset! server (srv/run-server (#'app opts) {:port available-port}))
    (println "Server started on Port: " available-port)))
