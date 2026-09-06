(ns dev
  "Serves public/ the way GitHub Pages will, rebuilds on save, reloads the browser."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [org.httpkit.server :as http]
            [selmer.util]
            [site]))

(def clients (atom #{}))
(def problem (atom nil))

(def script "<script>new EventSource('/events').onmessage = e => {
  if (e.data == 'reload') location.reload();
  else document.body.prepend(Object.assign(document.createElement('pre'), {textContent: e.data, style: 'background: #f8d7da; color: #000; padding: 1rem; white-space: pre-wrap'}));
}</script>")

(defn tell [ch msg] (http/send! ch (str "data: " msg "\n\n") false))

(defn events [req]
  (http/as-channel req {:on-open (fn [ch] (swap! clients conj ch)
                                   (http/send! ch {:status 200 :headers {"Content-Type" "text/event-stream"} :body ": hi\n\n"} false)
                                   (some->> @problem (tell ch)))
                        :on-close (fn [ch _] (swap! clients disj ch))}))

(defn file [f]
  {:status 200
   :headers {"Content-Type" (or (java.net.URLConnection/guessContentTypeFromName (str f)) "application/octet-stream")}
   :body (if (= "html" (fs/extension f)) (str (slurp (str f)) script) (fs/file f))})

(defn handler [{:keys [uri] :as req}]
  (let [f (fs/path site/public (java.net.URLDecoder/decode (subs uri 1) "UTF-8"))
        missing (fs/path site/public "404.html")]
    (cond (= uri "/events") (events req)
          (and (fs/directory? f) (not (str/ends-with? uri "/"))) {:status 301 :headers {"Location" (str uri "/")}}
          (fs/regular-file? (fs/path f "index.html")) (file (fs/path f "index.html"))
          (fs/regular-file? f) (file f)
          (fs/regular-file? missing) (assoc (file missing) :status 404)
          :else {:status 404 :headers {"Content-Type" "text/plain"} :body (str "not found: " uri)})))

(defn rebuild []
  (reset! problem (try (site/build :dev true) nil
                       (catch Exception e (str/replace (ex-message e) "\n" " "))))
  (when @problem (println "build failed:" @problem))
  (doseq [ch @clients] (tell ch (or @problem "reload"))))

(defn snapshot []
  (into {} (map (juxt str fs/last-modified-time))
        (mapcat #(fs/glob (fs/path vault/root %) "**") ["articles" "authors" "pages" "series" "decklists" "components" "assets"])))

(defn start [& [port]]
  (let [port (or (some-> port parse-long) 8080)]
    (selmer.util/set-missing-value-formatter! (fn [tag _] (if-let [v (:tag-value tag)] (str "<mark>missing " v "</mark>") "")))
    (rebuild)
    (http/run-server handler {:port port})
    (println (str "serving http://localhost:" port "/")))
  (loop [before (snapshot)]
    (Thread/sleep 200)
    (let [now (snapshot)]
      (when (not= before now) (rebuild))
      (recur now))))
