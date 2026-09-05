(ns site
  "Turns the vault into public/."
  (:require [babashka.fs :as fs]
            [clj-yaml.core :as yaml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [markdown]
            [selmer.filters :as filters]
            [selmer.parser :as selmer]
            [squint.compiler :as squint]))

(def root (fs/parent (fs/parent *file*)))
(def public (fs/path root "public"))
(def components (fs/path root "components"))
(selmer/set-resource-path! (str components))
(selmer/cache-off!)
(filters/add-filter! :host #(.getHost (java.net.URI. %)))

(defn fail [file msg] (throw (ex-info (str file ": " msg) {})))

(defn wall-clock [v]
  (if (inst? v) (java.time.LocalDateTime/ofInstant (.toInstant v) java.time.ZoneOffset/UTC) v))

(defn encode [path] (subs (.getRawPath (java.net.URI. nil nil (str "/" path) nil)) 1))

(defn read-note [dir f]
  (let [[_ fm body] (re-matches #"(?s)\A(?:---\r?\n(.*?)\r?\n---\r?\n)?(.*)" (slurp (str f)))
        slug (str (fs/strip-ext (fs/file-name f)))
        file (str (fs/relativize root f))
        props (try (update-vals (into {} (some-> fm yaml/parse-string)) wall-clock)
                   (catch Exception e (fail file (first (str/split-lines (ex-message e))))))
        path (cond (not= dir "pages") (str dir "/" slug "/") (= slug "hq") "" :else (str slug "/"))]
    (assoc props :slug slug :file file :body body :path path :url (encode path)
           :title (or (:title props) slug))))

(defn notes [dir] (sort-by :slug (map #(read-note dir %) (fs/glob (fs/path root dir) "**.md"))))

(defn check [authors {:keys [file author published-at tags] :as article}]
  (let [state (or (:state article) "draft")
        author (some-> author (str/replace #"^\[\[|\]\]$" ""))]
    (when-not (#{"draft" "review" "published"} state)
      (fail file (str "state is " (pr-str state) ", expected draft, review or published")))
    (when (and published-at (not (instance? java.time.LocalDateTime published-at)))
      (fail file (str "published-at is " (pr-str published-at) ", expected a datetime like 2026-09-01T12:00:00")))
    (when (and (= state "published") (not published-at))
      (fail file "a published article needs published-at"))
    (when (and (= state "published") (not (authors author)))
      (fail file (str "unknown author " (pr-str author) ", expected a note in authors/")))
    (assoc article :state state :author (authors author)
           :tags (if (string? tags) (str/split tags #",\s*") tags))))

(defn load-site [states]
  (let [authors (into {} (map (juxt :slug identity)) (notes "authors"))
        all (map #(check authors %) (notes "articles"))
        articles (->> all (filter #(states (:state %))) (sort-by :published-at #(compare %2 %1)) vec)
        pages (notes "pages")
        assets (filter fs/regular-file? (fs/glob (fs/path root "assets") "**"))]
    {:articles articles
     :authors (for [a (vals authors)]
                (assoc a :articles (filter #(= (:slug a) (:slug (:author %))) articles)))
     :pages (filter #(states (:state % "published")) pages)
     :by-series (for [[name as] (group-by :series articles) :when name]
                  {:name name :articles (sort-by :published-at as)})
     :index (into {} (concat (for [n (concat all (vals authors) pages)]
                               [(str/lower-case (:slug n)) (when (states (:state n "published")) (:url n))])
                             (for [f assets] [(str/lower-case (fs/file-name f)) (encode (str "assets/" (fs/file-name f)))])))}))

(defn linker
  "Vault name to site url. Drafts and typos give nil; only typos get a warning."
  [data file]
  (fn [name]
    (let [key (-> (fs/file-name name) str/lower-case (str/replace #"\.md$" ""))]
      (when-not (contains? (:index data) key)
        (println "warn:" file "links to" (pr-str name) "which is not in the vault"))
      (get-in data [:index key]))))

(defn render [component data]
  (try (selmer/render-file component data)
       (catch Exception e
         (let [file (fs/file-name (str (:template (ex-data e) component)))]
           (throw (ex-info (str "components/" file ": " (str/replace (ex-message e) #" for template file:.*" "")) {}))))))

(defn card [line]
  (if-let [[_ count name] (re-matches #"(\d+)x?\s+(.+)" line)] {:count count :name name} {:name line}))

(defn component
  "A fenced code block whose language names a component renders that component."
  [data {:keys [language text limit] :as fence}]
  (let [lines (remove str/blank? (str/split-lines text))]
    (if (fs/exists? (fs/path components (str language ".html")))
      (render (str language ".html") (cond-> (merge data fence {:lines lines :cards (map card lines)})
                                       limit (update :articles #(take (parse-long limit) %))))
      (when (and language (empty? lines))
        (println "warn:" (:file data) "has an empty" language "block and there is no" (str "components/" language ".html"))))))

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

(defn page
  "Renders one note. Gives back where it goes and the html."
  [data {:keys [path file body] :as note} component-name]
  (let [data (merge data note {:root (or (not-empty (str/replace path #"[^/]+/" "../")) "./")})
        data (assoc data :body (markdown/html body {:root (:root data) :url (linker data file) :component (partial component data)}))
        main (if component-name (render component-name data) (:body data))]
    [(fs/path public path "index.html") (-> (render "page.html" (assoc data :main main)) hoist (cljs (:root data)))]))

(defn build
  "Renders everything, then replaces public/. With :dev true, drafts are built too."
  [& {:keys [dev]}]
  (let [t0 (System/nanoTime)
        data (load-site (if dev #{"draft" "review" "published"} #{"published"}))
        pages (vec (concat (for [a (:articles data)] (page data a "article.html"))
                           (for [a (:authors data)] (page data a "author.html"))
                           (for [p (:pages data)] (page data p nil))))]
    (fs/delete-tree public)
    (doseq [[out html] pages]
      (fs/create-dirs (fs/parent out))
      (spit (str out) html))
    (fs/copy-tree (fs/path root "assets") (fs/path public "assets"))
    (fs/create-dirs (fs/path public "assets/squint"))
    (io/copy (io/input-stream (io/resource "squint/core.js")) (fs/file (fs/path public "assets/squint/core.js")))
    (println (format "built %d articles, %d authors, %d pages in %d ms"
                     (count (:articles data)) (count (:authors data)) (count (:pages data))
                     (quot (- (System/nanoTime) t0) 1000000)))))
