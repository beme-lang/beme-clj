(ns beme.alpha.runtime.cli-test
  "Integration tests for the beme CLI (subprocess-based).
   JVM-only (.clj) — invokes bb as a subprocess."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- bb
  "Run a bb command in the project root and return {:exit :out :err}."
  [& args]
  (let [pb (ProcessBuilder. ^java.util.List (into ["bb"] args))
        root (-> (io/resource "beme/alpha/version.cljc")
                 (.getPath)
                 (io/file)
                 (.getParentFile)   ; alpha/
                 (.getParentFile)   ; beme/
                 (.getParentFile)   ; src/
                 (.getParentFile))  ; project root
        _ (.directory pb root)
        _ (.redirectErrorStream pb false)
        proc (.start pb)
        out (slurp (.getInputStream proc))
        err (slurp (.getErrorStream proc))
        exit (.waitFor proc)]
    {:exit exit :out out :err err}))

;; ---------------------------------------------------------------------------
;; version
;; ---------------------------------------------------------------------------

(deftest cli-version
  (testing "beme version prints version string"
    (let [{:keys [exit out]} (bb "beme" "version")]
      (is (zero? exit))
      (is (str/starts-with? (str/trim out) "beme ")))))

;; ---------------------------------------------------------------------------
;; run
;; ---------------------------------------------------------------------------

(deftest cli-run-basic
  (testing "beme run executes a .beme file"
    (let [tmp (java.io.File/createTempFile "beme-cli-test" ".beme")]
      (try
        (spit tmp "println(\"cli-test-ok\")")
        (let [{:keys [exit out]} (bb "beme" "run" (str tmp))]
          (is (zero? exit))
          (is (str/includes? out "cli-test-ok")))
        (finally
          (.delete tmp))))))

(deftest cli-run-missing-file
  (testing "beme run with no file prints usage to stderr and exits 1"
    (let [{:keys [exit err]} (bb "beme" "run")]
      (is (= 1 exit))
      (is (str/includes? err "Usage")))))

(deftest cli-run-parse-error
  (testing "beme run with parse error exits 1 with error on stderr"
    (let [tmp (java.io.File/createTempFile "beme-cli-err" ".beme")]
      (try
        (spit tmp "foo(")
        (let [{:keys [exit err]} (bb "beme" "run" (str tmp))]
          (is (= 1 exit))
          (is (not (str/blank? err))))
        (finally
          (.delete tmp))))))

;; ---------------------------------------------------------------------------
;; convert
;; ---------------------------------------------------------------------------

(deftest cli-convert-beme-to-clj
  (testing "beme convert .beme --stdout produces Clojure"
    (let [tmp (java.io.File/createTempFile "beme-cli-conv" ".beme")]
      (try
        (spit tmp "+(1 2)")
        (let [{:keys [exit out]} (bb "beme" "convert" (str tmp) "--stdout")]
          (is (zero? exit))
          (is (str/includes? out "(+ 1 2)")))
        (finally
          (.delete tmp))))))

(deftest cli-convert-clj-to-beme
  (testing "beme convert .clj --stdout produces beme"
    (let [tmp (java.io.File/createTempFile "beme-cli-conv" ".clj")]
      (try
        (spit tmp "(+ 1 2)")
        (let [{:keys [exit out]} (bb "beme" "convert" (str tmp) "--stdout")]
          (is (zero? exit))
          (is (str/includes? out "+(1 2)")))
        (finally
          (.delete tmp))))))

(deftest cli-convert-no-args
  (testing "beme convert with no args prints usage and exits 1"
    (let [{:keys [exit out]} (bb "beme" "convert")]
      (is (= 1 exit))
      (is (str/includes? out "Usage")))))

;; ---------------------------------------------------------------------------
;; format
;; ---------------------------------------------------------------------------

(deftest cli-format-stdout
  (testing "beme format --stdout pretty-prints beme"
    (let [tmp (java.io.File/createTempFile "beme-cli-fmt" ".beme")]
      (try
        (spit tmp "defn(foo [x] +(x 1))")
        (let [{:keys [exit out]} (bb "beme" "format" (str tmp) "--stdout")]
          (is (zero? exit))
          (is (str/includes? out "defn")))
        (finally
          (.delete tmp))))))

(deftest cli-format-in-place
  (testing "beme format modifies file in place"
    (let [tmp (java.io.File/createTempFile "beme-cli-fmt" ".beme")]
      (try
        (spit tmp "defn(foo [x] +(x 1))")
        (let [{:keys [exit]} (bb "beme" "format" (str tmp))]
          (is (zero? exit))
          (let [result (slurp tmp)]
            (is (str/includes? result "defn"))))
        (finally
          (.delete tmp))))))

;; ---------------------------------------------------------------------------
;; help / unknown command
;; ---------------------------------------------------------------------------

(deftest cli-help
  (testing "beme with no args prints help"
    (let [{:keys [exit out]} (bb "beme")]
      (is (zero? exit))
      (is (str/includes? out "Commands")))))

(deftest cli-unknown-command
  (testing "beme with unknown command exits 1"
    (let [{:keys [exit err]} (bb "beme" "nosuchcmd")]
      (is (= 1 exit))
      (is (str/includes? err "Unknown command")))))
