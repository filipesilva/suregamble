(ns nrdb
  "Fetches cards and decklists from NetrunnerDB into notes."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [cheshire.core :as json]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [flatland.ordered.map :refer [ordered-map]]))

(def root (fs/parent (fs/parent *file*)))
(def api "https://api.netrunnerdb.com/api/v3/public/")

(defn fetch [url]
  (json/parse-string (:body (http/get url {:headers {"User-Agent" "catalyst"}})) true))

(defn fetch-all [path]
  (loop [url (str api path "?page[size]=1000") acc []]
    (let [{:keys [data links]} (fetch url) acc (into acc data)]
      (if-let [next (:next links)] (recur next acc) acc))))

(defn note-name
  "A card title as a file name Obsidian accepts."
  [title]
  (-> title (str/replace ": " " - ") (str/replace #"[\"*?<>|:\\#^\[\]]" "") (str/replace "/" "-")))

(defn number [v] (if (and (string? v) (re-matches #"\d+" v)) (parse-long v) v))

(def tokens {"credit" "credit" "click" "click" "subroutine" "↳" "trash" "trash" "mu" "MU"
             "interrupt" "interrupt" "recurring-credit" "recurring credit" "link" "link"})

(defn plain
  "Card text as markdown: icon tokens as words, html as markdown."
  [text]
  (-> (str text)
      (str/replace #"(\d+|X)\[credit\]" "$1 credits")
      (str/replace "1 credits" "1 credit")
      (str/replace #"(\[click\]){2,}" (fn [[m _]] (str (quot (count m) 7) " clicks")))
      (str/replace #"\[([a-z-]+)\]" (fn [[_ t]] (or (tokens t) (str/replace t "-" " "))))
      (str/replace #"</?strong>" "**")
      (str/replace #"</?em>" "*")
      (str/replace #"<ul>|</ul>|</li>" "")
      (str/replace "<li>" "\n- ")
      (str/replace "\n" "\n\n")))

(defn face
  "A face's image and text under headings, so ![[Card#Text]] can embed a part."
  [{:keys [title text]} image level]
  (str level " Image\n\n![" title "](" image ")\n\n" level " Text\n\n" (plain text) "\n"))

(defn card-note [{:keys [id attributes]}]
  (let [a attributes
        name (note-name (:title a))
        faces (:faces a)
        image #(get-in % [:nrdb_classic :large])
        props (ordered-map
               "title" (:title a)
               "aliases" (vec (remove #{name} (distinct (concat [(:title a) (:stripped_title a)] (map :title faces)))))
               "faces" (vec (map :title faces))
               "id" id
               "code" (:latest_printing_id a)
               "side" (:side_id a)
               "faction" (:faction_id a)
               "type" (:card_type_id a)
               "subtypes" (vec (:card_subtype_ids a))
               "cost" (number (:cost a))
               "strength" (:strength a)
               "influence" (:influence_cost a)
               "agenda-points" (:agenda_points a)
               "advancement" (number (:advancement_requirement a))
               "trash" (:trash_cost a)
               "memory" (:memory_cost a)
               "link" (:base_link a)
               "unique" (when (:is_unique a) true)
               "limit" (:deck_limit a)
               "influence-limit" (:influence_limit a)
               "deck-size" (:minimum_deck_size a)
               "set" (last (:card_set_names a))
               "released" (:date_release a)
               "formats" (vec (:format_ids a))
               "image" (image (:latest_printing_images a))
               "nrdb" (str "https://netrunnerdb.com/en/card/" (:latest_printing_id a)))
        props (into (ordered-map) (remove (fn [[_ v]] (or (nil? v) (and (coll? v) (empty? v))))) props)
        body (apply str (face a (image (:latest_printing_images a)) "##")
                    (for [f faces] (str "\n## " (:title f) "\n\n" (face f (image (:images f)) "###"))))]
    [name (str "---\n" (yaml/generate-string props :dumper-options {:flow-style :block}) "---\n" body)]))

(defn cards
  "Writes one note per card into cards/."
  []
  (let [dir (fs/path root "cards")]
    (fs/create-dirs dir)
    (let [notes (map card-note (fetch-all "cards"))]
      (doseq [[name text] notes] (spit (str (fs/path dir (str name ".md"))) text))
      (println "wrote" (count notes) "cards"))))

(defn card-index
  "Card id -> note name and properties, read from cards/."
  []
  (into {} (for [f (fs/glob (fs/path root "cards") "*.md")
                 :let [props (yaml/parse-string (second (re-find #"(?s)\A---\n(.*?)\n---" (slurp (str f)))))]]
             [(:id props) (assoc props :name (str (fs/strip-ext (fs/file-name f))))])))

(defn card [cards id]
  (or (cards id) (throw (ex-info (str "cards/ has no note for " id ", run bb cards") {}))))

(def sections
  {"runner" [["event" "Event"] ["hardware" "Hardware"] ["resource" "Resource"] ["icebreaker" "Icebreaker"] ["program" "Program"]]
   "corp" [["agenda" "Agenda"] ["asset" "Asset"] ["upgrade" "Upgrade"] ["operation" "Operation"]
           ["barrier" "Barrier"] ["code_gate" "Code Gate"] ["sentry" "Sentry"] ["multi" "Multi"] ["other" "Other"]]})

(defn section [{:keys [type subtypes]}]
  (let [subs (set subtypes)]
    (cond (= type "program") (if (subs "icebreaker") "icebreaker" "program")
          (= type "ice") (let [kinds (filter subs ["barrier" "code_gate" "sentry"])]
                           (cond (next kinds) "multi" (seq kinds) (first kinds) :else "other"))
          :else type)))

(defn link
  "A card link the way Obsidian writes one: the file name, shown as the title."
  [{:keys [name title]}]
  (str "[[" name (when (not= name title) (str "|" title)) "]]"))

(defn decklist
  "Writes a NetrunnerDB decklist as a note into decklists/."
  [url]
  (let [uuid (or (re-find #"[0-9a-f]{8}-[0-9a-f-]{27}" (str url)) (throw (ex-info "usage: bb decklist <netrunnerdb decklist url>" {})))
        {a :attributes} (:data (fetch (str api "decklists/" uuid)))
        cards (card-index)
        identity (card cards (:identity_card_id a))
        slots (for [[id n] (:card_slots a) :let [c (card cards (name id))] :when (not= c identity)] (assoc c :count n))
        agenda-points (reduce + (for [{:keys [count agenda-points]} slots :when agenda-points] (* count agenda-points)))
        groups (group-by section slots)
        summary (str (link identity) ", " (:num_cards a) " cards, " (:influence_spent a) " influence"
                     (when (pos? agenda-points) (str ", " agenda-points " agenda points")))
        body (for [[key label] (sections (:side_id a)) :let [rows (sort-by :title (groups key))] :when (seq rows)]
               (str "### " label " (" (reduce + (map :count rows)) ")\n"
                    (str/join "\n" (for [{:keys [count] :as c} rows] (str "- " count "x " (link c))))))
        text (str "---\n"
                  "title: " (json/generate-string (:name a)) "\n"
                  "nrdb: https://netrunnerdb.com/en/decklist/" uuid "\n"
                  "identity: \"[[" (:name identity) "]]\"\n"
                  "side: " (:side_id a) "\n"
                  "faction: " (:faction_id a) "\n"
                  "cards: " (:num_cards a) "\n"
                  "influence: " (:influence_spent a) "\n"
                  (when (pos? agenda-points) (str "agenda-points: " agenda-points "\n"))
                  "created: " (subs (:created_at a) 0 19) "\n"
                  "---\n"
                  summary "\n\n"
                  (str/join "\n\n" body) "\n")
        file (fs/path root "decklists" (str (note-name (:name a)) ".md"))]
    (fs/create-dirs (fs/parent file))
    (spit (str file) text)
    (println "wrote" (str (fs/relativize root file)))))
