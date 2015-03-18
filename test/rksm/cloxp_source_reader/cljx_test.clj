(ns rksm.cloxp-source-reader.cljx-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-source-reader.core :as src-rdr]
            [rksm.system-files.cljx :as cljx]))

(defn fixture [test]
  (cljx/enable-cljx-load-support!)
  (test)
  (remove-ns 'rksm.cloxp-source-reader.test.cljx-dummy))

(use-fixtures :each fixture)

(deftest read-cljx-intern-source
  (require 'rksm.cloxp-source-reader.test.cljx-dummy :reload)
  (let [meta-data (src-rdr/add-source-to-interns
                   'rksm.cloxp-source-reader.test.cljx-dummy
                   (map meta (vals (ns-interns 'rksm.cloxp-source-reader.test.cljx-dummy))))
        sources (map :source meta-data)
        expected '("(defn x-to-string\n  [x]\n  (let [buf #+clj (StringBuilder.) #+cljs (gstring/StringBuffer.)]\n    (.append buf \"x is: \")\n    (.append buf (str x))))\n"
                   "(def x 23)\n"
                   "(def y #+clj 24 #+cljx 25)")]
    (is (= expected sources))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 (keep (comp (partial re-find #".*repl.*") str) (rksm.system-files/classpath))
 (run-tests *ns*)
  )
