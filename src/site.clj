(ns site
  "Turns the vault into public/."
  (:require [babashka.fs :as fs]
            [clj-yaml.core :as yaml]
            [clojure.java.io :as io]
            [hiccup2.core :as h]
            [clojure.string :as str]
            [markdown]
            [nrdb]
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

(defn vault-key
  "How Obsidian matches a link: the file name, lower-cased, without .md."
  [name]
  (-> (fs/file-name name) str/lower-case (str/replace #"\.md$" "")
      (java.text.Normalizer/normalize java.text.Normalizer$Form/NFC)))

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

(defn folder
  "Folders of notes read on demand: cards/ has two thousand and a page needs a few."
  [& dirs]
  {:cache (atom {})
   :files (into {} (for [dir dirs, f (fs/glob (fs/path root dir) "*.md")] [(vault-key f) [dir f]]))})

(defn lookup
  "A vault name, or a card title, to its note."
  [{:keys [files cache]} name]
  (when-let [[dir f] (get files (str/lower-case (nrdb/note-name (vault-key name))))]
    (or (@cache f) (let [n (read-note dir f)] (swap! cache assoc f n) n))))

(defn load-site [states]
  (let [authors (into {} (map (juxt :slug identity)) (notes "authors"))
        all (map #(check authors %) (notes "articles"))
        articles (->> all (filter #(states (:state %))) (sort-by :published-at #(compare %2 %1)) vec)
        pages (notes "pages")
        assets (filter fs/regular-file? (fs/glob (fs/path root "assets") "**"))]
    {:articles articles
     :cards (folder "cards")
     :decks (folder "decklists")
     :vault (folder "articles" "authors" "pages")
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
    (let [key (vault-key name)]
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

(defn section
  "The markdown under a heading, for ![[note#heading]]. Nested headings as a#b, matched like Obsidian, case insensitive."
  [body path]
  (reduce (fn [text heading]
            (when text
              (let [level (fn [line] (count (re-find #"^#+(?=\s)" line)))
                    wanted? (fn [line] (and (pos? (level line))
                                            (= (str/lower-case heading) (str/lower-case (str/trim (subs line (level line)))))))
                    [_ [start & after]] (split-with (complement wanted?) (str/split-lines text))]
                (when start
                  (str/join "\n" (cons start (take-while #(or (zero? (level %)) (> (level %) (level start))) after)))))))
          body (str/split path #"#")))

(defn card-link
  "A card name as a link to NetrunnerDB with a hover preview: the image, then the text of every face."
  [card & [label]]
  (let [text (markdown/inline (str/replace (:body card) #"(?m)^(#+ (Image|Text)|!\[.*?\))\n?" ""))]
    (str/trimr (render "card.html" (assoc card :label (or label (:title card)) :text text)))))

(defn card-image [card width]
  (str (h/html [:a {:href (:nrdb card)} [:img {:src (:image card) :alt (:title card) :width (or width 300)}]])))

(defn deck-sections
  "The deck note's body: ### headings, then - 3x [[Card]] lines."
  [cards {:keys [file body]}]
  (reduce (fn [sections line]
            (cond (re-find #"^### " line) (conj sections {:name (subs line 4) :rows []})
                  (re-find #"^- (\d+)x \[\[([^\]|]+)" line)
                  (let [[_ n name] (re-find #"^- (\d+)x \[\[([^\]|]+)" line)
                        card (lookup cards name)]
                    (when-not card (println "warn:" file "lists" (pr-str name) "which is not in cards/"))
                    (update-in sections [(dec (count sections)) :rows] conj {:count (parse-long n) :name name :card card}))
                  (str/starts-with? line "- ") (fail file (str "cannot read deck line " (pr-str line)))
                  :else sections))
          [{:name nil :rows []}]
          (str/split-lines body)))

(defn decklist [cards deck]
  (let [name (str/replace (str (:identity deck)) #"^\[\[|\]\]$" "")
        identity (or (lookup cards name) (fail (:file deck) (str "identity " (pr-str name) " is not a note in cards/")))
        row (fn [{:keys [count name card] :as r}]
              (assoc r :link (if card (card-link card) name)
                       :faction (:faction card)
                       :dots (when (and card (not= (:faction card) (:faction identity)) (pos? (:influence card 0)))
                               (apply str (repeat (* count (:influence card)) "●")))))
        sections (for [s (deck-sections cards deck) :when (seq (:rows s))] (update s :rows #(map row %)))
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

(defn page
  "Renders one note. Gives back where it goes and the html."
  [data {:keys [path file body] :as note} component-name]
  (let [data (merge data note {:root (or (not-empty (str/replace path #"[^/]+/" "../")) "./")})
        {:keys [cards decks vault]} data
        ctx (atom nil)
        md (fn [text] (markdown/html text @ctx))]
    (reset! ctx {:root (:root data) :url (linker data file) :component (partial component data)
                 :note-link (fn [name label] (some-> (lookup cards name) (card-link label)))
                 :note-embed (fn [name width]
                               (let [[note heading] (str/split name #"#" 2)]
                                 (if heading
                                   (some-> (or (lookup decks note) (lookup cards note) (lookup vault note)) :body (section heading) md)
                                   (or (some->> (lookup decks note) (decklist cards))
                                       (some-> (lookup cards note) (card-image width))))))})
    (let [data (assoc data :body (md body))
          main (if component-name (render component-name data) (:body data))]
      [(fs/path public path "index.html") (-> (render "page.html" (assoc data :main main)) hoist (cljs (:root data)))])))

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
