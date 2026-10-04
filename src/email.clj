(ns email
  "Emails each newly published article to subscribers through Sender (https://api.sender.net).
   Sender holds the state: each article gets one campaign, titled with its url."
  (:require [babashka.http-client :as http]
            [cheshire.core :as json]
            [site])
  (:import [org.jsoup Jsoup]))

(def api "https://api.sender.net/v2/")
(def group "eXAzpk")
(def from "Sure Gamble")
(def reply-to "newsletter@suregamble.net")
(def window (java.time.Duration/ofDays 2))

(defn request [method url & [body]]
  (let [{:keys [status] :as res} (http/request {:method method :uri url :throw false
                                                :headers {"Authorization" (str "Bearer " (System/getenv "SENDER_TOKEN"))
                                                          "Content-Type" "application/json" "Accept" "application/json"}
                                                :body (some-> body json/generate-string)})]
    (when (>= status 400)
      (throw (ex-info (str (name method) " " url " gave " status ": " (:body res)) {})))
    (json/parse-string (:body res) true)))

(defn campaigns []
  (loop [url (str api "campaigns?limit=100") acc []]
    (let [{:keys [data links]} (request :get url) acc (into acc data)]
      (if-let [next (:next links)] (recur next acc) acc))))

(defn recent
  "Articles published within the last `window`, reading published-at as UTC. Older ones are never emailed, future ones are not live yet."
  [articles]
  (let [now (java.time.LocalDateTime/now java.time.ZoneOffset/UTC)]
    (filter #(and (.isBefore (:published-at %) now) (.isAfter (:published-at %) (.minus now window))) articles)))

(defn pending
  "Recent articles without a sent campaign, each with its draft if one was left behind. Only asks Sender when there are recent articles."
  [data]
  (when-let [recent (seq (recent (:articles data)))]
    (let [by-title (into {} (map (juxt :title identity)) (campaigns))]
      (for [a (reverse recent)
            :let [title (str site/site (:url a)) campaign (by-title title)]
            :when (or (nil? campaign) (= "DRAFT" (:status campaign)))]
        (assoc a :campaign-title title :draft campaign)))))

(def faction-colors
  "The light colors of components/decklist.html, since mail apps do not support light-dark()."
  {"anarch" "orangered" "criminal" "royalblue" "shaper" "limegreen" "haas_bioroid" "blueviolet"
   "jinteki" "crimson" "nbn" "darkorange" "weyland_consortium" "darkgreen" "adam" "#a89c33"
   "apex" "black" "sunny_lebeau" "#66686b" "neutral_runner" "gray" "neutral_corp" "gray"})

(defn content
  "The feed content with the site's image sizing and influence colors inlined, since mail apps do not have the site css."
  [data article]
  (let [doc (Jsoup/parseBodyFragment (site/feed-content data article))]
    (.attr (.select doc "img") "style" "max-width: 100%; height: auto;")
    (doseq [dots (.select doc ".influence")]
      (when-let [color (some faction-colors (.classNames dots))]
        (.attr dots "style" (str "color: " color ";"))))
    (.html (.body doc))))

(defn create [data article]
  (:data (request :post (str api "campaigns")
                  {:title (:campaign-title article)
                   :subject (:title article)
                   :preheader (:summary article)
                   :from from
                   :reply_to reply-to
                   :content_type "html"
                   :content (site/render "email.html" (assoc article :site site/site :content (content data article)))
                   :groups [group]})))

(defn run
  "Lists what would be emailed, or with send true, emails it."
  [send]
  (let [data (site/load-site #{"published"})]
    (doseq [a (pending data)]
      (apply println (if send "sending" "would send") (:campaign-title a) (when (:draft a) ["(draft left behind)"]))
      (when send
        (let [{:keys [id]} (or (:draft a) (create data a))]
          (request :post (str api "campaigns/" id "/send")))))))
