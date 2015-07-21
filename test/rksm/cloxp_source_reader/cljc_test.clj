(ns rksm.cloxp-source-reader.cljc-test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-source-reader.core :as src-rdr]
            [clojure.java.io :as io]
            [rksm.system-files :as sf]))

(defn fixture [test]
  (test)
  (remove-ns 'rksm.cloxp-source-reader.test.cljc-dummy))

(use-fixtures :each fixture)

(deftest read-cljc-intern-source
  (require 'rksm.cloxp-source-reader.test.cljc-dummy :reload)
  (let [meta-data (src-rdr/add-source-to-interns
                   'rksm.cloxp-source-reader.test.cljc-dummy
                   (map meta (vals (ns-interns 'rksm.cloxp-source-reader.test.cljc-dummy))))
        sources (map :source meta-data)
        expected '("(defn x-to-string\n  [x]\n  (let [buf #?(:clj (StringBuilder.) :cljs (gstring/StringBuffer.))]\n    (.append buf \"x is: \")\n    (.append buf (str x))))\n"
                   "(def x 23)\n"
                   "(def y #?(:clj 24 :cljs 25))\n")]
    (is (= expected sources))))

(deftest read-objs-cljc
  (let [result (src-rdr/read-objs "(def y #?(:clj 24 :cljs 25))")
        expected '({:name y,
                    :form "(def y #?(:clj 24 :cljs 25))",
                    :source "(def y #?(:clj 24 :cljs 25))\n",
                    :line 1,
                    :column 1,
                    :end-line 2,
                    :end-column 1})]
    (is (= expected
           (update-in result [0 :form] pr-str)))))

(deftest
    ^{:doc "This test was meaningful for cljx to see if sf/file can output
      strip the fie content to output only valid clj or cljs. For cljc all
      tooling (cljs analyzer + compiler as well as the clojure compiler seem to
      be able to deal with it pure. I'll keep it around for now in case we
      still need it."}
    cljc-file-reading

  #_(let [path (str (.getParentFile (sf/file-for-ns 'rksm.cloxp-source-reader.cljc-test))
                  "/test/cljc_dummy.cljc")
        real-content (slurp (io/file path))
        ; clj-content (cljc.core/transform real-content cljc.rules/clj-rules)
        ; cljs-content (cljc.core/transform real-content cljc.rules/cljs-rules)
        file (sf/file path)
        read-normal (slurp file)
        ; read-clj (binding [cljx-file/*output-mode* :clj] (slurp file))
        ; read-cljs (binding [cljx-file/*output-mode* :cljs] (slurp file))
        ]
      (testing "output mode binding"
        (is (= real-content read-normal))
        (is (= clj-content read-clj))
        (is (= cljs-content read-cljs)))
      
      (testing "changeMode"
        (.changeMode file :clj) (is (= clj-content (slurp file)))
        (is (= cljs-content (slurp (cljx-file/with-mode path :cljs)))))
      
      (testing "changeMode and binding"
        (is (= clj-content 
               (binding [cljx-file/*output-mode* :clj]
                 (slurp (cljx-file/with-mode path :cljs))))))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 (keep (comp (partial re-find #".*repl.*") str) (rksm.system-files/classpath))
 (let [s (java.io.StringWriter.)]
   (binding [*test-out* s] 
     (test-ns *ns*)
     (print (str s))))
  )
