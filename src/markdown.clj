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

(defn wikilink [{:keys [root url]} {:keys [text]}]
  (let [[target alias] (str/split text #"\|" 2)
        [note heading] (str/split target #"#" 2)
        label (or alias (if heading (str note " > " heading) note))]
    (if-let [target (url note)]
      [:a {:href (str root target (some->> heading anchor (str "#")))} label]
      [:span.unresolved label])))

(defn image [{:keys [root url] :as ctx} {:keys [name width]}]
  (if-not (re-find #"(?i)\.(png|jpe?g|gif|svg|webp)$" name)
    (wikilink ctx {:text name})
    (if-let [target (url name)]
      [:img {:src (str root target) :alt (str/replace name #"\.\w+$" "") :width width}]
      [:span.unresolved name])))

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
         :internal-link wikilink :embed image :blockquote callout :code code
         :hashtag (fn [{:keys [root]} {:keys [text]}] [:a.tag {:href (str root "archives/?q=" text)} (str "#" text)])
         :html-block (fn [_ node] (h/raw (md/node->text node)))
         :html-inline (fn [_ node] (h/raw (md/node->text node)))))

(defn strip-comments [src]
  (str/replace src #"(?s)(```.*?```)|%%.*?%%" (fn [[_ code]] (or code ""))))

(defn html
  "ctx carries :root, :url (vault name -> url, or nil) and :component (fence -> html, or nil)."
  [src ctx]
  (->> (strip-comments src)
       (md/parse {:text-tokenizers [embed u/internal-link-tokenizer tag]})
       (md/->hiccup (merge renderers ctx))
       h/html str))
