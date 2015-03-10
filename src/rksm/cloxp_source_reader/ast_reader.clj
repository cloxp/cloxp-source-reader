(ns rksm.cloxp-source-reader.ast-reader
  (:require [clojure.tools.analyzer.ast :as ana-ast]
            [clojure.tools.analyzer.jvm :as ana-jvm]
            [clojure.set :as set]
            [rksm.cloxp-source-reader.core :refer (read-objs)]))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
; parsing via tools.analyzer

(defn- find-defs
  [expr]
   (let [child-result (mapcat find-defs (ana-ast/children expr))
         my-result (if (= :def (:op expr))
                     (->> expr ((juxt :env #(hash-map :name (:name %)))) (apply merge)))]
     (if my-result 
       (conj child-result my-result)
       child-result)))

(defn- file-loc-id
  [{line :line, column :column}]
  {:line line, :column column})

(defn- merge-read-objs-with-ast
  [ast-defs read-objs]
  (let [indexed-a (apply merge (map #(hash-map (file-loc-id %) %) ast-defs))
        indexed-b (apply merge (map #(hash-map (file-loc-id %) %) read-objs))
        ids-both (set/intersection (-> indexed-a keys set) (-> indexed-b keys set))
        a (map (partial get indexed-a) ids-both)
        b (map (partial get indexed-b) ids-both)]
    (->> (concat a b)
      (group-by file-loc-id)
      vals
      (map (partial apply merge)))))

(defn read-and-parse
  [src namespace]
  (let [read (read-objs src)
        forms (map :form read)
        ast (ana-jvm/analyze
             (list forms)
             {:context :eval, :locals {}, :ns namespace})
        defs (find-defs ast)]
    (merge-read-objs-with-ast defs read)))
