(ns rksm.cloxp-source-reader.core
  (:require [clojure.tools.reader.reader-types :as trt]
            [clojure.tools.reader :as tr]
            [clojure.string :as s]
            [rksm.system-files :refer (source-reader-for-ns)])
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT)))

(defn purge-string!
  [rdr]
  (let [buf (-> rdr .rdr .source_log_frames var-get :buffer)
        str (.toString buf)]
    (.delete buf 0 (count str))
    str))

(defn read-objs
  "Reads sexps from rdr-or-src and returns them as a {:form :source :line
  :column} map. Note: this is more that the typical reader gives us."
  [rdr-or-src]
  ; FIXME this is hacked...
  (let [rdr (trt/indexing-push-back-reader (trt/source-logging-push-back-reader rdr-or-src))]
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
                column (+ start-column (count (second first-line-ws-match)))]
            (when (= \newline (trt/peek-char rdr))
              (trt/read-char rdr)
              (purge-string! rdr))
            (recur (conj result {:form o ;(with-meta o (assoc (meta o) :source src))
                                 :source src
                                 :line line
                                 :column column})))
          result)))))

(defn- read-next-obj
  "follows the reader while it core/reads an object and returns the string in
  range of what was read"
  [rdr]
  (let [text (StringBuilder.) 
        pbr (proxy [PushbackReader] [rdr]
                   (read []
                         (let [i (proxy-super read)]
                           (if (> i -1) (.append text (char i)))
                           i)))]
    (if (= :unknown *read-eval*)
      (throw (IllegalStateException. "Unable to read source while *read-eval* is :unknown."))
      (tr/read (PushbackReader. pbr) false nil))
    (str text)))

(defn- read-entity-source
  "goes forward in line numbering reader until line of entity is reached and
  reads that as an object"
  [{lrdr :lrdr, sources :sources, :as record} meta-entity]
  (or (if-let [line (:line meta-entity)]
        (do
          (dotimes [_ (dec (- line (.getLineNumber lrdr)))] (.readLine lrdr))
          (let [new-meta (merge meta-entity {:source (read-next-obj lrdr)})]
            (update-in record [:sources] conj new-meta))))
      record))

(defn add-source-to-interns-with-reader
  "interns are supposed to be meta-data-like maps, at least including :line for
  the entity to be read"
  [rdr interns]
  (let [source-data {:lrdr (LineNumberReader. rdr), :sources []}]
    (if-let [result (reduce read-entity-source source-data interns)]
      (:sources result)
      interns)))

(defn add-source-to-interns
  "alternative for `source-for-symbol`. Instead of using clojure.repl this
  functions uses the classloader info provided by system-files to find more
  recent versions of the source.
  NOTE: If there are multiple versions a lib on the classpath than it is
  possible that this function will retrieve code that i not actually in the
  system! (and the system meta data will clash with the actual file contents)"
  [ns interns & [ns-file-path]]
  (if-let [rdr (source-reader-for-ns ns ns-file-path)]
    (with-open [rdr rdr]
      (add-source-to-interns-with-reader rdr interns))
    interns))

(defn add-source-to-interns
  "alternative for `source-for-symbol`. Instead of using clojure.repl this
  functions uses the classloader info provided by system-files to find more
  recent versions of the source.
  NOTE: If there are multiple versions a lib on the classpath than it is
  possible that this function will retrieve code that i not actually in the
  system! (and the system meta data will clash with the actual file contents)"
  [ns interns & [ns-file-path]]
  (if-let [rdr (source-reader-for-ns ns ns-file-path)]
    (with-open [rdr rdr]
      (add-source-to-interns-with-reader rdr interns))
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
