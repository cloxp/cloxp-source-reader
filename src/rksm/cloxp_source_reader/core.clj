(ns rksm.cloxp-source-reader.core
  (:require [clojure.tools.reader.reader-types :as trt]
            [clojure.tools.reader.impl.utils]
            [clojure.tools.reader :as tr]
            [clojure.tools.namespace.parse :as tnp]
            [clojure.string :as s]
            [rksm.system-files :refer (source-reader-for-ns)]
            (cljx core rules))
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT)))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; string helper

(defn- line-column-access
  [string]
  (let [lines (s/split-lines string)]
    (fn line-column-access-for-string
      [{start-line :line start-column :column :as s} {end-line :line end-column :column :as e}]
      (if (or (> start-line end-line) (and (= start-line end-line) (> start-column end-column)))
        (line-column-access-for-string e s)
        (let [start  (nth lines (dec start-line))
              start (.substring start (dec start-column))
              end  (if (= start-line end-line)
                     start
                     (nth lines (min (dec (count lines)) (dec end-line))))
              end (.substring end 0 (if (= start-line end-line) (- (dec end-column) (dec start-column)) (dec end-column)))
              inbetween (->> lines (drop start-line) (take (dec (- end-line start-line))))]
          (s/join "\n"
                  (if (= start-line end-line)
                    [end]
                    (concat [start] inbetween [end]))))))))

(comment
 (def s "hello\nworld\n\nfoo\nbar\n\n")
 ((line-column-access s) {:line 3 :column 1} {:line 2 :column 3})
 ((line-column-access s) {:line 1 :column 5} {:line 1 :column 4})
 ((line-column-access s) {:line 2 :column 3} {:line 3 :column 1})
 ((line-column-access s) {:line 1 :column 1} {:line 1 :column 2})
 )

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn- make-reader
  "Currently using direct instantiation of reader b/c there seems to be a bug
  in tools reader regarding line number access, see comment"
  [source & [file-name]]
  (comment
   ; this is a bug!!
   (assert (= 1 (.get_line_number
                 (trt/source-logging-push-back-reader "")))))
  (trt/->SourceLoggingPushbackReader
    (trt/string-push-back-reader source 1)
    1 ; line <-- bug here, 0 or 1???
    1 ; column
    true ; line-start?
    nil ; prev
    0 ; prev-column
    file-name ; file-name
    (let [anon-var (with-local-vars [x nil] x)]
     (doto anon-var
       (alter-var-root (constantly {:buffer (StringBuilder.)
                                    :offset 0}))))))

(defn name-of-def
  [form]
  (first (drop 1 (filter symbol? form))))

(defn def?
  [form]
  (and (seq? form)
       (boolean (re-find #"(^|\/)def" (str (first form))))))

(defn defmethod?
  [form]
  (and (seq? form)
       (= 'defmethod (first form))))

(defn defmulti?
  [form]
  (and (seq? form)
       (= 'defmulti (first form))))

(defn defmethod-qualifier
  "takes a defmethod form and extract the match args from it, like
  '(defmethod ^{:dynamic true}foo-method String [::foo \"Bar\"] ([x] (.toUpperCase x)))
  =>
  '(String [:user/foo \"Bar\"])"
  [form]
  (if (defmethod? form)
    (let [ex-form (macroexpand form)
          [_ _ _ match-1 fn-def] ex-form
          rest-matches (if (= (->> fn-def last (map type))
                              [clojure.lang.PersistentVector clojure.lang.PersistentList])
                         (->> fn-def (drop 1) (drop-last))
                         (->> fn-def (drop 1) (drop-last 2)))]
      (cons match-1 rest-matches))))

(defn defmethod-qualifier-string
  [form]
  (clojure.string/join "-" (defmethod-qualifier form)))

(defn purge-string!
  [rdr]
  (let [buf (-> rdr .source_log_frames var-get :buffer)
        str (.toString buf)]
    (.delete buf 0 (count str))
    str))

(defn read-with-source-logger
  "reads a single next obj from *current-code* :source"
  [src]
  (let [rdr (make-reader src)]
    (tr/read rdr)))

(def src-access (atom nil))

(defn read-objs
  "Reads sexps from source and returns them as a {:form :source :line
  :column} map. Note: this is more that the typical reader gives us."
  [source & [{:keys [cljx? line-offset column-offset] :or {cljx? true} :as opts}]]
  ; FIXME this is hacked...
  (let [source (if-not (.endsWith source "\n") (str source "\n") source)
        tfm-source (if cljx? (cljx.core/transform source cljx.rules/clj-rules) source)
        get-src-fn (line-column-access source)
        rdr (make-reader tfm-source)
        line-offset (or line-offset 0)
        column-offset (or column-offset 0)]
    (loop [result []]
      (let [start-line (trt/get-line-number rdr)
            start-column (trt/get-column-number rdr)]
        (if-let [o (tr/read rdr false nil)]
          (let [; get the string from the reader:
                raw-str (purge-string! rdr)
                lines (s/split-lines raw-str)
                ; trim surrounding whitespace and offset line / column accordingly
                ws-lines (take-while #(re-find #"^\s*(;.*)?$" %) lines)
                src-lines (drop (count ws-lines) lines)
                [_ leading-ws first-line-content] (re-matches #"^(\s*)(.*)" (first src-lines))
                src-lines (assoc (vec src-lines) 0 first-line-content)
                src (s/join "\n" src-lines)
                line (+ start-line (count ws-lines))
                column (+ (if (> (count ws-lines) 0) 1 start-column) (count leading-ws))
                meta (meta o)
                def? (def? o)
                defmethod? (if def? (defmethod? o))
                name (if def? (name-of-def o))
                defmethod-name (if defmethod? (defmethod-qualifier-string o))]
            (when (= \newline (trt/peek-char rdr))
              (trt/read-char rdr)
              (purge-string! rdr))
            (let [start {:line line :column column}
                  start-for-meta {:line (+ line line-offset)
                                  :column (if (= 1 start-line) (+ column column-offset) column)}
                  end-line (trt/get-line-number rdr)
                  end-column (trt/get-column-number rdr)
                  end {:line end-line :column end-column}
                  end-for-meta {:end-line (+ end-line line-offset)
                                :end-column (if (= 1 start-line) (+ end-column column-offset) end-column)}
                  source (get-src-fn start end)]
              (recur (conj result
                           (merge start-for-meta
                                  end-for-meta
                                  {:form o, :source source}
                                  (if def?
                                    {:form (with-meta o (assoc meta :source src)),
                                     :name name})
                                  (if defmethod?
                                    {:defmethod-qualifier defmethod-name}))))))
          result)))))

(defn add-source-to-interns-with-reader
  "interns are supposed to be meta-data-like maps, at least including :line for
  the entity to be read"
  [rdr interns & [{:keys [cljx?] :as opts}]]
  {:pre [every? #((or (contains? (:line %))
                      (contains? (:name %)))) interns]}
  (let [file-source (slurp rdr)
        get-src-fn (line-column-access file-source)
        interns (sort-by :line interns)
        clj-source (if cljx?
                     (cljx.core/transform file-source cljx.rules/clj-rules)
                     file-source)
        objs (doall (read-objs clj-source))
        obj-map (apply sorted-map (mapcat (juxt :name identity) objs))]
    (sort-by :line (keep (fn [{n :name l :line c :column, :as meta-entity}]
                           (if-let [{:keys [line column end-line end-column source]}
                                    (if n
                                      (get obj-map n)
                                      (->> objs
                                        (filter (fn [{c2 :column, l2 :line}] (and (= c c2) (= l l2))))
                                        first))]
                             ;   (assoc meta-entity :source source)
                             (assoc meta-entity :source
                                    (get-src-fn
                                     {:line line :column column}
                                     {:line end-line :column end-column}))))
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
  (let [file (rksm.system-files/file-for-ns ns file)
        cljx? (or cljx? (and (nil? cljx?) (boolean (re-find #"\.cljx$" (str file)))))]
    (if-let [rdr (source-reader-for-ns ns file)]
      (with-open [rdr rdr]
        (add-source-to-interns-with-reader rdr interns {assoc opts :cljx? cljx?}))
      interns)))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn updated-source
  "Takes the new source for a def and produces a new version of the ns source,
  with the new def code embedded. meta-info is a meta-data like structure."
  [sym {:keys [line] :as meta-info} new-src-for-def old-src-for-def file-src]
  (let [lines (s/split-lines file-src)
        line (dec line)
        before-lines (take line lines)
        after-lines (-> old-src-for-def
                      s/trim-newline
                      s/split-lines count
                      (drop (drop line lines)))]
    (str (s/join "\n" (concat before-lines [(s/trim-newline new-src-for-def)] after-lines)))))

(defn read-ns-decl
  [source-or-rdr]
  (let [rdr (if (instance? java.io.PushbackReader source-or-rdr)
              source-or-rdr
              (-> source-or-rdr
                java.io.StringReader.
                java.io.PushbackReader.))]
    (tnp/read-ns-decl rdr)))

(defn read-ns-sym
  [source-or-rdr]
  (some-> source-or-rdr read-ns-decl second))

(comment
 (read-ns-sym "foo\n(ns ^{:doc \"baz\"} bar)")
 )
