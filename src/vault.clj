(ns vault
  "Reads notes: the frontmatter as properties, the rest as body."
  (:require [babashka.fs :as fs]
            [clj-yaml.core :as yaml]
            [clojure.string :as str]))

(def root (fs/parent (fs/parent *file*)))

(defn fail [file msg] (throw (ex-info (str file ": " msg) {})))

(defn link-key
  "How Obsidian matches a link: the file name, lower-cased, without .md."
  [name]
  (-> (fs/file-name name) str/lower-case (str/replace #"\.md$" "")
      (java.text.Normalizer/normalize java.text.Normalizer$Form/NFC)))

(defn property
  "A datetime as the wall-clock time typed, a [[link]] as the note it names."
  [v]
  (cond (inst? v) (java.time.LocalDateTime/ofInstant (.toInstant v) java.time.ZoneOffset/UTC)
        (string? v) (if-let [[_ name] (re-matches #"\[\[([^\]|]+)(?:\|[^\]]*)?\]\]" v)] name v)
        :else v))

(defn read-note [dir f]
  (let [[_ fm body] (re-matches #"(?s)\A(?:---\r?\n(.*?)\r?\n---\r?\n)?(.*)" (slurp (str f)))
        slug (str (fs/strip-ext (fs/file-name f)))
        file (str (fs/relativize root f))
        props (try (update-vals (into {} (some-> fm yaml/parse-string)) property)
                   (catch Exception e (fail file (first (str/split-lines (ex-message e))))))]
    (assoc props :dir dir :slug slug :key (link-key f) :file file :body body :title (or (:title props) slug))))

(defn notes
  "Every note in a folder, subfolders included."
  [dir]
  (sort-by :slug (map #(read-note dir %) (fs/glob (fs/path root dir) "**.md"))))

(defn folder
  "Notes read on demand: cards/ has two thousand and a page needs a few."
  [& dirs]
  {:cache (atom {})
   :files (into {} (for [dir dirs, f (fs/glob (fs/path root dir) "**.md")] [(link-key f) [dir f]]))})

(defn lookup [{:keys [files cache]} name]
  (when-let [[dir f] (get files (link-key name))]
    (or (@cache f) (let [n (read-note dir f)] (swap! cache assoc f n) n))))
