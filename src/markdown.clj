(ns markdown
  "Obsidian flavoured markdown to HTML."
  (:require [clojure.string :as str]
            [hiccup2.core :as h]
            [nextjournal.markdown :as md]
            [nextjournal.markdown.utils :as u]))

(def tag (assoc u/hashtag-tokenizer :regex #"(?U)(^|\B)#(?=[\w/-]*[^\W\d])[\w/-]+"))

(def embed {:regex #"!\[\[([^\]|]+)(?:\|(\d+))?\]\]"
            :handler (fn [[_ name width]] {:type :embed :name name :width width})})

(defn anchor [heading] (-> heading str/lower-case (str/replace #"\s+" "-")))

(defn hook [ctx k & args] (some-> (get ctx k) (apply args) h/raw))

(defn wikilink [{:keys [root url] :as ctx} {:keys [text]}]
  (let [[target alias] (str/split text #"\|" 2)
        [note heading] (str/split target #"#" 2)
        label (or alias (if heading (str note " > " heading) note))]
    (or (hook ctx :note-link note label)
        (if-let [target (url note)]
          [:a {:href (str root target (some->> heading anchor (str "#")))} label]
          [:span.unresolved label]))))

(defn image [{:keys [root url] :as ctx} {:keys [name width]}]
  (or (hook ctx :note-embed name width)
      (if-not (re-find #"(?i)\.(png|jpe?g|gif|svg|webp)$" name)
        (wikilink ctx {:text name})
        (if-let [target (url name)]
          [:img {:src (str root target) :alt (str/replace name #"\.\w+$" "") :width width}]
          [:span.unresolved name]))))

(defn paragraph [ctx {:keys [content] :as node}]
  (if (and (= 1 (count content)) (= :embed (:type (first content))))
    (image ctx (first content))
    ((:paragraph md/default-hiccup-renderers) ctx node)))

(defn callout [ctx {:keys [content] :as node}]
  (let [[{text :text} & more] (:content (first content))
        [_ kind fold title] (when (string? text) (re-find #"^\[!(\w+)\]([+-]?)\s*(.*)" text))
        [title-nodes [_ & first-line]] (split-with #(not= :softbreak (:type %)) more)
        title (if (and (empty? title) (empty? title-nodes)) (str/capitalize (str kind)) title)
        heading (md/into-hiccup (if (empty? fold) [:p.callout-title] [:summary]) ctx
                                {:content (into [{:type :text :text title}] title-nodes)})
        body {:content (cond->> (rest content) (seq first-line) (cons {:type :paragraph :content first-line}))}]
    (cond (not kind) (md/into-hiccup [:blockquote] ctx node)
          (empty? fold) (md/into-hiccup [:aside.callout {:data-callout (str/lower-case kind)} heading] ctx body)
          :else (md/into-hiccup [:details.callout {:data-callout (str/lower-case kind) :open (= fold "+")} heading] ctx body))))

(defn code [{:keys [component] :as ctx} node]
  (or (some-> (component (assoc node :text (md/node->text node))) h/raw)
      ((:code md/default-hiccup-renderers) ctx node)))

(def renderers
  (assoc md/default-hiccup-renderers
         :internal-link wikilink :embed image :blockquote callout :code code :paragraph paragraph
         :hashtag (fn [{:keys [root]} {:keys [text]}] [:a.tag {:href (str root "archives/?q=" text)} (str "#" text)])
         :html-block (fn [_ node] (h/raw (md/node->text node)))
         :html-inline (fn [_ node] (h/raw (md/node->text node)))))

(defn strip-comments [src]
  (str/replace src #"(?s)(```.*?```)|%%.*?%%" (fn [[_ code]] (or code ""))))

(defn inline
  "Markdown as inline html, for text that lives inside a link: blocks become spans."
  [src]
  (let [span (fn [class] (fn [ctx node] (md/into-hiccup [:span {:class class}] ctx node)))]
    (->> (md/parse src)
         (md/->hiccup (assoc renderers :doc (span "doc") :paragraph (span "p") :heading (span "h")
                             :bullet-list (span "list") :list-item (span "item") :url (constantly nil)))
         h/html str)))

(defn html
  "ctx carries :root, :url (vault name -> url, or nil), :component (fence -> html, or nil),
   :note-link (name, label -> html, or nil) and :note-embed (name, width -> html, or nil)."
  [src ctx]
  (->> (strip-comments src)
       (md/parse {:text-tokenizers [embed u/internal-link-tokenizer tag]})
       (md/->hiccup (merge renderers ctx))
       h/html str))
