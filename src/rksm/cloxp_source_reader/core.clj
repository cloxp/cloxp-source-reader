(ns rksm.cloxp-source-reader.core
  (:require [clojure.tools.reader.reader-types :as trt]
            [clojure.tools.reader :as tr]
            [clojure.string :as s]
            [rksm.system-files :refer (source-reader-for-ns)]
            (cljx core rules))
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT)))

(defn name-of-def
  [form]
  (first (drop 1 (filter symbol? form))))

(defn def?
  [form]
  (and (seq? form)
       (->> form first str (re-find #"(^|\/)def") boolean)))

(defn purge-string!
  [rdr]
  (let [buf (-> rdr .rdr .source_log_frames var-get :buffer)
        str (.toString buf)]
    (.delete buf 0 (count str))
    str))

(defn read-with-source-logger
  "reads a single next obj from *current-code* :source"
  [src]
  (let [rdr (trt/source-logging-push-back-reader src)]
    (tr/read rdr)))

(defn read-objs
  "Reads sexps from rdr-or-src and returns them as a {:form :source :line
  :column} map. Note: this is more that the typical reader gives us."
  [rdr-or-src]
  ; FIXME this is hacked...
  (let [rdr (trt/indexing-push-back-reader
             (trt/source-logging-push-back-reader
              rdr-or-src))]
    (loop [result []]
      (let [start-line (trt/get-line-number rdr)
            start-column (trt/get-column-number rdr)]
        (if-let [o (tr/read rdr false nil)]
          (let [raw-str (purge-string! rdr)
                lines (s/split-lines raw-str)
                no-ws-lines (take-while #(re-find #"^\s*(;.*)?$" %) lines)
                src-lines (drop (count no-ws-lines) lines)
                first-line-ws-match (re-matches #"^(\s*)(.*)" (first src-lines))
                src-lines (assoc (vec src-lines) 0 (nth first-line-ws-match 2))
                src (s/join "\n" src-lines)
                line (+ (count no-ws-lines) start-line)
                column (+ start-column (count (second first-line-ws-match)))
                meta (meta o)
                def? (def? o)
                name (if def? (name-of-def o))]
            (when (= \newline (trt/peek-char rdr))
              (trt/read-char rdr)
              (purge-string! rdr))
            (recur (conj result
                         (merge {:form o, :source src,
                                 :line line, :column column,
                                 :end-line (trt/get-line-number rdr),
                                 :end-column (trt/get-column-number rdr)}
                                (if def?
                                  {:form (with-meta o (assoc meta :source src)),
                                   :name name})))))
          result)))))

(defn add-source-to-interns-with-reader
  "interns are supposed to be meta-data-like maps, at least including :line for
  the entity to be read"
  [rdr interns & [opts]]
  {:pre [every? #((or (contains? (:line %)) 
                      (contains? (:name %)))) interns]}
  (let [objs (read-objs (slurp rdr))
        obj-map (apply sorted-map (mapcat (juxt :name identity) objs))]
    (sort-by
     :line
     (keep (fn [{n :name l :line c :column, :as meta-entity}]
             (if-let [{s :source}
                      (if n
                        (get obj-map n)
                        (->> objs
                          (filter (fn [{c2 :column, l2 :line}] (and (= c c2) (= l l2))))
                          first))]
               (assoc meta-entity :source s)))
           interns))))

(defn add-source-to-interns
  "alternative for `source-for-symbol`. Instead of using clojure.repl this
  functions uses the classloader info provided by system-files to find more
  recent versions of the source.
  NOTE: If there are multiple versions a lib on the classpath than it is
  possible that this function will retrieve code that i not actually in the
  system! (and the system meta data will clash with the actual file contents)"
  [ns interns & [{:keys [file cljx?]} :as opts]]
  [file cljx?]
  (if-let [rdr (source-reader-for-ns ns file)]
    (let [cljx? (or cljx? (and
                           (nil? cljx?)
                           (string? file)
                           (re-find #"\.cljx$" file)))
          rdr (if cljx? 
                (->  (slurp rdr)
                  (cljx.core/transform cljx.rules/clj-rules)
                  java.io.StringReader.)
                rdr)]
      (with-open [rdr rdr]
        (add-source-to-interns-with-reader rdr interns opts)))
    interns))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

; (defn add-source-to-interns-from-repl
;   "This method uses the RT/baseloader to lookup the files belonging to symbols.
;   When files get reloaded / defs redefined this can mean that the code being
;   retrieved is outdated"
;   [ns intern-meta-data]
;   (let [ns-string (str (ns-name ns))
;         sym-fn (partial symbol ns-string)
;         source-fn #(or (source-for-symbol (sym-fn (-> % :name str))) "")]
;     (map #(assoc % :source (source-fn %)) intern-meta-data)))
