(ns site
  "Turns the vault into public/."
  (:require [babashka.fs :as fs]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [markdown]
            [selmer.filters :as filters]
            [selmer.parser :as selmer]
            [squint.compiler :as squint]
            [vault :refer [fail lookup]])
  (:import [org.jsoup Jsoup]))

(def site "https://filipesilva.github.io/catalyst/")
(def public (fs/path vault/root "public"))
(def components (fs/path vault/root "components"))
(selmer/set-resource-path! (str components))
(filters/add-filter! :host #(.getHost (java.net.URI. %)))

(defn encode [path] (subs (.getRawPath (java.net.URI. nil nil (str "/" path) nil)) 1))

(defn slugify
  "A note name as a url: ascii letters and digits, dashes between."
  [name]
  (-> (java.text.Normalizer/normalize name java.text.Normalizer$Form/NFD)
      (str/replace #"\p{M}" "") str/lower-case (str/replace #"[^a-z0-9]+" "-") (str/replace #"^-|-$" "")))

(defn locate
  "Where a note goes: folder and slug, pages at the root, hq is the root, 404 is 404.html."
  [{:keys [dir slug] :as note}]
  (let [slug (slugify slug)
        path (cond (not= dir "pages") (str dir "/" slug "/") (= slug "hq") "" (= slug "404") "404.html" :else (str slug "/"))]
    (assoc note :path path :url (encode path))))

(defn notes [dir] (map locate (vault/notes dir)))

(defn check [authors series {:keys [file author published-at tags] :as article}]
  (let [state (or (:state article) "draft")]
    (when-not (#{"draft" "review" "published"} state)
      (fail file (str "state is " (pr-str state) ", expected draft, review or published")))
    (when (and published-at (not (instance? java.time.LocalDateTime published-at)))
      (fail file (str "published-at is " (pr-str published-at) ", expected a datetime like 2026-09-01T12:00:00")))
    (when (and (= state "published") (not published-at))
      (fail file "a published article needs published-at"))
    (when (and (= state "published") (not (authors author)))
      (fail file (str "unknown author " (pr-str author) ", expected a note in authors/")))
    (when (and (:series article) (not (series (:series article))))
      (fail file (str "unknown series " (pr-str (:series article)) ", expected a note in series/")))
    (assoc article :state state :author (authors author) :series (series (:series article))
           :tags (if (string? tags) (str/split tags #",\s*") tags))))

(defn load-site [states]
  (let [authors (into {} (map (juxt :slug identity)) (notes "authors"))
        series (into {} (map (juxt :slug identity)) (notes "series"))
        all (map #(check authors series %) (notes "articles"))
        articles (->> all (filter #(states (:state %))) (sort-by :published-at #(compare %2 %1)) vec)
        pages (notes "pages")
        assets (filter fs/regular-file? (fs/glob (fs/path vault/root "assets") "**"))]
    {:articles articles
     :site site
     :notes (vault/folder "cards" "decklists" "articles" "authors" "pages" "series")
     :authors (for [a (vals authors)]
                (assoc a :articles (filter #(= (:slug a) (:slug (:author %))) articles)))
     :all-series (sort-by :title (for [s (vals series)
                                       :let [parts (filter #(= (:slug s) (:slug (:series %))) articles)]
                                       :when (seq parts)
                                       :let [parts (sort-by :published-at parts)]]
                                   (assoc s :articles parts :latest (reverse (take-last 3 parts)))))
     :pages (filter #(states (:state % "published")) pages)
     :index (into {} (concat (for [n (concat all (vals authors) (vals series) pages)]
                               [(:key n) (when (states (:state n "published")) (:url n))])
                             (for [f assets] [(vault/link-key f) (encode (str "assets/" (fs/file-name f)))])))}))

(defn linker
  "Vault name to site url. Drafts and typos give nil; only typos get a warning."
  [data file]
  (fn [name]
    (let [key (vault/link-key name)]
      (when-not (contains? (:index data) key)
        (println "warn:" file "links to" (pr-str name) "which is not in the vault"))
      (get-in data [:index key]))))

(defn render [component data]
  (try (selmer/render-file component data)
       (catch Exception e
         (let [file (fs/file-name (str (:template (ex-data e) component)))]
           (throw (ex-info (str "components/" file ": " (str/replace (ex-message e) #" for template file:.*" "")) {}))))))

(defn component
  "A fenced code block whose language names a component renders that component."
  [data {:keys [language text limit] :as fence}]
  (if (fs/exists? (fs/path components (str language ".html")))
    (render (str language ".html") (cond-> (merge data fence) limit (update :articles #(take (parse-long limit) %))))
    (when (and language (str/blank? text))
      (println "warn:" (:file data) "has an empty" language "block and there is no" (str "components/" language ".html")))))

(defn card-text
  "The text of every face, read through the note's own #Text headings."
  [{:keys [body faces]}]
  (let [text (fn [path] (str/replace (markdown/section body path) #"\A.*\n+" ""))]
    (str/join "\n\n" (cons (text "Text") (for [f faces] (str "## " f "\n\n" (text (str f "#Text"))))))))

(defn card-link
  "A card name as a link to NetrunnerDB with a hover preview."
  [card & [label]]
  (str/trimr (render "card.html" (assoc card :label (or label (:title card)) :text (markdown/inline (card-text card))))))

(defn card-image [card width]
  (str/trimr (render "card-image.html" (assoc card :width width))))

(defn deck-sections
  "The deck note's body: ### headings, then - 3x [[Card]] lines."
  [notes {:keys [file body]}]
  (reduce (fn [sections line]
            (let [[_ n name] (re-find #"^- (\d+)x \[\[([^\]|]+)" line)]
              (cond (str/starts-with? line "### ") (conj sections {:name (subs line 4) :rows []})
                    n (let [card (lookup notes name)]
                        (when-not card (println "warn:" file "lists" (pr-str name) "which is not in cards/"))
                        (update-in sections [(dec (count sections)) :rows] conj {:count (parse-long n) :name name :card card}))
                    (str/starts-with? line "- ") (fail file (str "cannot read deck line " (pr-str line)))
                    :else sections)))
          [{:name nil :rows []}]
          (str/split-lines body)))

(defn decklist [notes deck]
  (let [identity (or (lookup notes (:identity deck))
                     (fail (:file deck) (str "identity " (pr-str (:identity deck)) " is not a note in cards/")))
        row (fn [{:keys [count name card] :as r}]
              (assoc r :link (if card (card-link card) name)
                       :dots (when (and card (not= (:faction card) (:faction identity)) (pos? (:influence card 0)))
                               (apply str (repeat (* count (:influence card)) "●")))))
        sections (for [s (deck-sections notes deck) :when (seq (:rows s))] (update s :rows #(map row %)))
        right? (fn [s] (contains? #{"program" "ice"} (:type (:card (first (:rows s))))))]
    (render "decklist.html" (assoc deck :identity (assoc identity :link (card-link identity))
                                        :columns [(remove right? sections) (filter right? sections)]))))

(defn hoist
  "Moves the <style> blocks components bring along into <head>, once each."
  [html]
  (let [styles (map #(str/replace % #"(?m)^" "  ") (distinct (re-seq #"(?s)<style>.*?</style>" html)))]
    (-> (str/replace html #"(?s)\n?<style>.*?</style>" "")
        (str/replace "</head>" (str/join "\n" (concat styles ["</head>"]))))))

(defn cljs
  "Compiles <script type=\"application/cljs\"> blocks to JavaScript."
  [html root]
  (str/replace html #"(?s)<script type=\"application/cljs\">(.*?)</script>"
               (fn [[_ src]]
                 (str "<script type=\"module\">\nimport * as squint_core from \"" root "assets/squint/core.js\";\n"
                      (:body (squint/compile-string* src {:elide-imports true :elide-exports true}))
                      "</script>"))))

(defn tidy
  "Pretty prints a page."
  [html]
  (.outerHtml (Jsoup/parse html)))

(defn page
  "Renders one note. Gives back where it goes and the html."
  [data {:keys [path file body] :as note} component-name]
  (let [{:keys [notes]} data
        root (if (= path "404.html") "/" (or (not-empty (str/replace path #"[^/]+/" "../")) "./"))
        data (merge data note {:root root})
        card (fn [name] (let [n (lookup notes name)] (when (= "cards" (:dir n)) n)))
        body (markdown/html body {:root (:root data) :url (linker data file) :component (partial component data)
                                  :note-link (fn [name label] (some-> (card name) (card-link label)))
                                  :note-body (fn [name] (:body (lookup notes name)))
                                  :note-embed (fn [name width]
                                                (let [n (lookup notes name)]
                                                  (case (:dir n) "decklists" (decklist notes n) "cards" (card-image n width) nil)))})
        data (assoc data :body body)
        main (if component-name (render component-name data) body)]
    [(if (str/ends-with? path ".html") (fs/path public path) (fs/path public path "index.html"))
     (-> (render "page.html" (assoc data :main main)) hoist (cljs root) tidy)]))

(defn build
  "Renders everything, then replaces public/. With :dev true, drafts are built too."
  [& {:keys [dev]}]
  (selmer/clear-cache!)
  (let [t0 (System/nanoTime)
        data (load-site (if dev #{"draft" "review" "published"} #{"published"}))
        rendered (vec (concat (for [a (:articles data)] (page data a "article.html"))
                              (for [a (:authors data)] (page data a "author.html"))
                              (for [s (:all-series data)] (page data s nil))
                              (for [p (:pages data)] (page data p nil))))]
    (fs/delete-tree public)
    (doseq [[out html] rendered]
      (fs/create-dirs (fs/parent out))
      (spit (str out) html))
    (spit (str (fs/path public "feed.xml")) (render "feed.xml" data))
    (fs/copy-tree (fs/path vault/root "assets") (fs/path public "assets"))
    (fs/create-dirs (fs/path public "assets/squint"))
    (io/copy (io/input-stream (io/resource "squint/core.js")) (fs/file (fs/path public "assets/squint/core.js")))
    (println (format "built %d articles, %d authors, %d series, %d pages in %d ms"
                     (count (:articles data)) (count (:authors data)) (count (:all-series data)) (count (:pages data))
                     (quot (- (System/nanoTime) t0) 1000000)))))
