(ns rksm.cloxp-source-reader.test
  (:require [clojure.test :refer :all]
            [rksm.cloxp-source-reader.core :as src-rdr]
            [rksm.cloxp-source-reader.ast-reader :as ast-rdr]
            (rksm.cloxp-source-reader.test dummy-1 dummy-3)))

(defmacro codify
  [& body]
  `(clojure.string/join "\n" (map str '~body)))

(deftest source-reader-matches-interns
  (remove-ns 'rksm.cloxp-source-reader.test.dummy-3)
  (require 'rksm.cloxp-source-reader.test.dummy-3 :reload)
  (let [src (slurp (clojure.java.io/resource "rksm/cloxp_source_reader/test/dummy_3.clj"))
        infos (into [{:ns (find-ns 'rksm.cloxp-source-reader.test.dummy-3),
                      :name 'non-existing,
                      :file "rksm/cloxp_source_reader/test.clj",
                      :end-column 14, :end-line 2,
                      :column 13, :line 1}]
                    (map meta (vals (ns-interns 'rksm.cloxp-source-reader.test.dummy-3))))
        ; infos (map meta (vals (ns-interns 'rksm.cloxp-source-reader.test.dummy-3)))

        result (src-rdr/add-source-to-interns-with-reader (java.io.StringReader. src) infos)
        expected ['x 'dummy-atom 'test-func 'foo]]
    (is (= expected (map :name result)))))

(deftest ast-reader-reading

  (testing "simple read"
    (is (= [{:form '(ns rksm.cloxp-source-reader.test.dummy-3),
             :source "(ns rksm.cloxp-source-reader.test.dummy-3)\n",
             :line 1, :column 1
             :end-line 2, :end-column 1}
            {:form '(def x 23), :source "(def x 23)\n",
             :name 'x
             :line 2, :column 3,
             :end-line 3, :end-column 1}]
         (src-rdr/read-objs "(ns rksm.cloxp-source-reader.test.dummy-3)\n  (def x 23)\n")))))

(deftest ast-reader-parsing

  (testing "parse source"
    (let [src "(ns rksm.cloxp-source-reader.test.dummy-3)\n  (defmacro b [] `~23)\n(+ 2 3)\n(defn foo [] `~23)\n"
          expected [{:ns 'rksm.cloxp-source-reader.test.dummy-3,
                     :name 'foo,
                     :source "(defn foo [] `~23)\n",
                     :line 4
                    ;  :column 1 :end-line 3, :end-column 1
                     }
                    {:ns 'rksm.cloxp-source-reader.test.dummy-3,
                     :name 'b,
                     :source "(defmacro b [] `~23)\n"
                     :line 2,
                    ;  :column 1, :end-line 3, :end-column 1
                     }]]
      (is (= expected
             (map #(select-keys % [:name :ns :source :line])
                  (ast-rdr/read-and-parse src 'rksm.cloxp-source-reader.test.dummy-3)))))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(deftest source-retrieval
;   (testing "get source for intern"
;     (is (= "(def x 23)"
;           (source-for-symbol 'rksm.cloxp-source-reader.test.dummy-1/x))))

  (testing "extract meta entities from source"

    (testing "meta entities match source"
      (is (= [{:source "(def x 23)\n" :column 1,:line 1}
              {:source "(def y 24)\n" :column 1,:line 2}]
             (let [entities [{:column 1,:line 1} {:column 1,:line 2}]
                   source (java.io.StringReader. "(def x 23)\n(def y 24)\n")]
               (src-rdr/add-source-to-interns-with-reader source entities)))))

    (testing "less meta entities than source"
      (is (= [{:source "(def x 23)\n" :column 1,:line 1}
              {:source "(def y 24)\n" :column 1,:line 6}]
             (let [entities [{:column 1,:line 1} {:column 1,:line 6}]
                   source (java.io.StringReader. "(def x 23)\n(def baz\n\n99)\n\n(def y 24)\n")]
               (src-rdr/add-source-to-interns-with-reader source entities)))))

    (testing "more meta entities than source"
      (is (= [{:source "(def x 23)\n" :column 1,:line 1}]
             (let [entities [{:column 1,:line 1} {:column 1,:line 6}]
                   source (java.io.StringReader. "(def x 23)")]
               (src-rdr/add-source-to-interns-with-reader source entities)))))

    (testing "not entities in source"
      "this might be kind of unexpected but the reader des not care bout lines"
      (is (= [{:source "(def y 24)\n" :column 1,:line 3}]
             (let [entities [{:column 1,:line 3} {:column 1,:line 6}]
                   source (java.io.StringReader. "(def x 23)\n\n(def y 24)")]
               (src-rdr/add-source-to-interns-with-reader source entities)))))))

(deftest multimethod-reading
  (is (= '(nil nil ":a")
         (map :defmethod-qualifier
              (src-rdr/read-objs (codify (ns multi-test-1)
                                         (defmulti multi-f (fn [x & _] x))
                                         (defmethod multi-f :a [_ x] (+ x 3))))))))

(deftest ns-decl-read-test
  (is (= 'bar (src-rdr/read-ns-sym "foo\n(ns ^{:doc \"baz\"} bar)"))))

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
 (test-ns 'rksm.cloxp-source-reader.test)
 )