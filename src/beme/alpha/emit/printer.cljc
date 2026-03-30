(ns beme.alpha.emit.printer
  "beme printer: Clojure forms → beme text."
  (:require [clojure.string :as str]
            [beme.alpha.forms :as forms]))

;; ---------------------------------------------------------------------------
;; Forward declaration
;; ---------------------------------------------------------------------------

(declare print-form)

;; ---------------------------------------------------------------------------
;; Surface syntax helpers
;; ---------------------------------------------------------------------------

(def ^:private infix-prec-table
  {"or" 10 "and" 20
   "=" 30 "not=" 30 "<" 30 ">" 30 "<=" 30 ">=" 30
   "+" 40 "-" 40 "*" 50 "/" 50 "mod" 50 "rem" 50})

(defn- infix-prec-of [form]
  (when (seq? form)
    (let [h (first form)]
      (when (symbol? h)
        (get infix-prec-table (name h))))))

(defn- print-infix-arg
  "Print a form that is an argument to an infix operator.
   Wraps in parens if arg is an infix op with lower precedence (to preserve semantics)."
  [parent-op arg _right?]
  (let [parent-prec (get infix-prec-table (name parent-op))
        arg-prec    (infix-prec-of arg)]
    (if (and arg-prec (< arg-prec parent-prec))
      (str "(" (print-form arg) ")")
      (print-form arg))))

;; When true, lists print in Clojure S-expression style: (f x y)
;; instead of beme call style: f(x y). Set by the quote handler
;; so that '(...) contains Clojure syntax. Also used by pprint.
(def ^:dynamic *clj-mode* false)

;; When false, surface syntax (infix operators, block forms) is suppressed.
;; Set to false when printing M-expression call arguments so that nested
;; arithmetic like (+ 1 2) prints as +(1 2) not 1 + 2, ensuring roundtrip
;; correctness (parse-forms-until does not apply Pratt parsing inside calls).
(def ^:dynamic *surface-context* true)

;; ---------------------------------------------------------------------------
;; Print helpers
;; ---------------------------------------------------------------------------

(defn- print-args
  "Print a sequence of forms separated by spaces.
   Suppresses surface syntax so that nested arithmetic uses M-expression style
   (e.g. (+ 1 2) → +(1 2) not 1 + 2), ensuring roundtrip correctness."
  [forms]
  (binding [*surface-context* false]
    (str/join " " (map print-form forms))))

(defn- percent-param?
  "Is sym a % parameter symbol (%1, %2, %&)?"
  [sym]
  (and (symbol? sym)
       (let [n (name sym)]
         (or (= n "%&")
             (and (str/starts-with? n "%")
                  (> (count n) 1)
                  (re-matches #"\d+" (subs n 1)))))))

(defn- max-percent-n
  "Find the max numbered %N param index referenced in a form body.
   Returns max N found (0 if none). Ignores %& (rest params).
   Skips nested (fn ...) bodies — their % params are scoped to the inner fn."
  [form]
  (cond
    (symbol? form)
    (let [n (name form)]
      (if (and (str/starts-with? n "%") (> (count n) 1)
               (re-matches #"\d+" (subs n 1)))
        #?(:clj (Long/parseLong (subs n 1))
           :cljs (js/parseInt (subs n 1) 10))
        0))
    (and (seq? form) (= 'fn (first form))) 0
    (seq? form) (reduce max 0 (map max-percent-n form))
    (vector? form) (reduce max 0 (map max-percent-n form))
    (map? form) (reduce max 0 (mapcat (fn [[k v]] [(max-percent-n k) (max-percent-n v)]) form))
    (set? form) (reduce max 0 (map max-percent-n form))
    #?@(:clj [(tagged-literal? form) (max-percent-n (.-form form))])
    :else 0))

;; ---------------------------------------------------------------------------
;; #() shorthand detection
;; ---------------------------------------------------------------------------

(defn- anon-fn-shorthand?
  "Can (fn [params] body) be printed as #(body)?
   True when: single-body, all params are %-style, and declared param
   count matches body usage (avoids silently changing arity)."
  [form]
  (and (seq? form)
       (= 'fn (first form))
       (= 3 (count form))
       (vector? (second form))
       (let [params (second form)]
         (and (every? percent-param? params)
              (let [declared (count (filter #(not= (name %) "%&") params))]
                (= declared (max-percent-n (nth form 2))))))))

;; ---------------------------------------------------------------------------
;; Main dispatch
;; ---------------------------------------------------------------------------

(defn print-form
  "Print a single Clojure form as beme text."
  [form]
  (cond
    ;; metadata prefix: ^:key, ^Type, or ^{map} — emit before the form
    ;; Filter out :line/:column/:file added by Clojure's compiler/reader
    (and (some? form)
         #?(:clj (instance? clojure.lang.IMeta form)
            :cljs (satisfies? IMeta form))
         (some? (meta form))
         (seq (dissoc (meta form) :line :column :file :ws)))
    (let [m (dissoc (meta form) :line :column :file :ws)
          stripped (with-meta form nil)
          prefix (cond
                   ;; single true-valued keyword: ^:key
                   (and (= 1 (count m))
                        (keyword? (key (first m)))
                        (true? (val (first m))))
                   (str "^" (print-form (key (first m))))
                   ;; single :tag with symbol value: ^Type
                   (and (= 1 (count m))
                        (contains? m :tag)
                        (symbol? (:tag m)))
                   (str "^" (print-form (:tag m)))
                   ;; general map
                   :else
                   (str "^" (print-form m)))]
      (str prefix " " (print-form stripped)))

    ;; nil
    (nil? form) "nil"

    ;; boolean
    (boolean? form) (str form)

    ;; Deferred auto-resolve keywords: (clojure.core/read-string "::foo") → ::foo
    (forms/deferred-auto-keyword? form)
    (forms/deferred-auto-keyword-raw form)

    ;; empty list — in beme mode, print as '() (bare () is invalid beme);
    ;; in clj-mode (inside quoted lists), print as ()
    (and (seq? form) (empty? form))
    (if *clj-mode* "()" "'()")

    ;; sequences — calls and reader sugar
    (seq? form)
    (if *clj-mode*
      ;; In clj-mode (inside quoted lists), print as S-expressions
      (str "(" (str/join " " (map print-form form)) ")")
      (let [head (first form)]
        (cond
          (anon-fn-shorthand? form)
          (str "#(" (print-form (nth form 2)) ")")

          ;; @deref
          (= head 'clojure.core/deref) (str "@" (print-form (second form)))

          ;; 'quote — quoted lists use Clojure S-expression syntax inside.
          ;; Activates *clj-mode* so nested lists print as (f x) not f(x).
          (= head 'quote)
          (let [inner (second form)]
            (if (not (seq? inner))
              (str "'" (print-form inner))
              (binding [*clj-mode* true]
                (str "'(" (str/join " " (map print-form inner)) ")"))))

          ;; #'var
          (= head 'var) (str "#'" (print-form (second form)))

          ;; --- Surface syntax rendering ---
          ;; Only applies when *surface-context* is true (not inside M-expression call args).

          ;; Infix binary operators — only in surface context
          (and *surface-context*
               (symbol? head)
               (contains? #{"+" "-" "*" "/" "mod" "rem"
                             "=" "not=" "<" ">" "<=" ">="
                             "and" "or"} (name head))
               (= 3 (count form)))
          (let [op    (name head)
                left  (second form)
                right (nth form 2)
                left-s  (print-infix-arg head left false)
                right-s (print-infix-arg head right true)]
            (str left-s " " op " " right-s))

          ;; if: (if test then) or (if test then else) — surface block form
          ;; Uses " :" (space before colon) so tokenizer separates them correctly.
          (and *surface-context* (= head 'if) (>= (count form) 3))
          (let [[_ test & body] form
                mid             (count body)
                then-forms      (if (> mid 1) (butlast body) body)
                else-forms      (when (> mid 1) [(last body)])]
            (str "if " (print-form test) " :\n"
                 (str/join "\n" (map #(str "  " (print-form %)) then-forms))
                 (when else-forms
                   (str "\nelse :\n"
                        (str/join "\n" (map #(str "  " (print-form %)) else-forms))))
                 "\nend"))

          ;; when/when-not: (when test body...)
          (and *surface-context* (#{' when 'when-not} head) (>= (count form) 2))
          (let [[_ test & body] form]
            (str (name head) " " (print-form test) " :\n"
                 (str/join "\n" (map #(str "  " (print-form %)) body))
                 "\nend"))

          ;; let: (let [x 1] body...) — single-binding uses surface block form.
          ;; Multi-binding falls through to M-expression call below (let([...] ...)).
          ;; This ensures multi-binding let roundtrips structurally correctly.
          (and *surface-context*
               (= head 'let) (vector? (second form))
               (= 2 (count (second form)))  ; exactly one binding pair
               (> (count form) 2))
          (let [[_ bindings & body] form
                [k v] bindings]
            (str "let " (print-form k) " := " (print-form v) " :\n"
                 (str/join "\n" (map #(str "  " (print-form %)) body))
                 "\nend"))

          ;; defn/defn-/defmacro: single-arity with vector params
          ;; Note: "):\" uses ")" before ":" so tokenizer splits correctly.
          (and *surface-context*
               (#{' defn 'defn- 'defmacro} head)
               (symbol? (second form))
               (vector? (nth form 2 nil))
               (> (count form) 3))
          (let [[_ fname params & body] form]
            (str (name head) " " (print-form fname)
                 "(" (str/join ", " (map print-form params)) ") :\n"
                 (str/join "\n" (map #(str "  " (print-form %)) body))
                 "\nend"))

          ;; fn: (fn [params] body...) — anonymous function without & rest params
          ;; Only use surface form when params are plain symbols (no & rest).
          ;; Always use block form (with " :") since the short form "fn(x) expr"
          ;; is ambiguous with M-expression call "fn(x)" followed by expression.
          (and *surface-context*
               (= head 'fn) (not (anon-fn-shorthand? form))
               (vector? (second form))
               (not (some #{'&} (second form)))
               (> (count form) 2))
          (let [[_ params & body] form]
            (str "fn(" (str/join ", " (map print-form params)) ") :\n"
                 (str/join "\n" (map #(str "  " (print-form %)) body))
                 "\nend"))

          ;; for/doseq/dotimes: surface block form
          (and *surface-context*
               (#{' for 'doseq 'dotimes} head)
               (vector? (second form))
               (> (count form) 2))
          (let [[_ bindings & body] form
                pairs (partition 2 bindings)
                bind-str (str/join ", " (map (fn [[t s]]
                                               (if (keyword? t)
                                                 (str (name t) " " (print-form s))
                                                 (str (print-form t) " in " (print-form s))))
                                             pairs))]
            (str (name head) " " bind-str " :\n"
                 (str/join "\n" (map #(str "  " (print-form %)) body))
                 "\nend"))

          ;; cond: (cond test1 val1 test2 val2 ...)
          (and *surface-context* (= head 'cond) (>= (count form) 3))
          (let [pairs (partition 2 (rest form))]
            (str "cond :\n"
                 (str/join "\n"
                           (map (fn [[t v]]
                                  (str "  " (if (= t :else) "else" (print-form t))
                                       " => " (print-form v)))
                                pairs))
                 "\nend"))

          ;; case: (case expr val1 result1 val2 result2 ... [default])
          (and *surface-context* (= head 'case) (>= (count form) 3))
          (let [[_ dispatch & rest-forms] form
                ;; Last element might be default (odd count of rest-forms)
                [pairs default] (if (odd? (count rest-forms))
                                  [(partition 2 (butlast rest-forms)) (last rest-forms)]
                                  [(partition 2 rest-forms) nil])]
            (str "case " (print-form dispatch) " :\n"
                 (str/join "\n"
                           (map (fn [[v r]] (str "  " (print-form v) " => " (print-form r)))
                                pairs))
                 (when default (str "\n  else => " (print-form default)))
                 "\nend"))

          ;; do: (do body...)
          (and *surface-context* (= head 'do) (> (count form) 1))
          (let [[_ & body] form]
            (str "do :\n"
                 (str/join "\n" (map #(str "  " (print-form %)) body))
                 "\nend"))

          ;; call: (f args...) → f(args...) when head is a symbol
          (symbol? head)
          (str (print-form head) "(" (print-args (rest form)) ")")

          ;; keyword-headed list: (:require [bar]) → :require([bar])
          (keyword? head)
          (str (print-form head) "(" (print-args (rest form)) ")")

          ;; vector-headed list: ([params] body) → [params](body)
          (vector? head)
          (str (print-form head) "(" (print-args (rest form)) ")")

          ;; set-headed list: (#{:a :b} x) → #{:a :b}(x)
          (set? head)
          (str (print-form head) "(" (print-args (rest form)) ")")

          ;; map-headed list: ({:a 1} :a) → {:a 1}(:a)
          (map? head)
          (str (print-form head) "(" (print-args (rest form)) ")")

          ;; any other head — reader conditionals, tagged literals, etc.
          ;; may be valid call heads via maybe-call on opaque forms
          :else
          (str (print-form head) "(" (print-args (rest form)) ")"))))

    ;; vector
    (vector? form)
    (str "[" (str/join " " (map print-form form)) "]")

    ;; map
    (map? form)
    (str "{"
         (str/join " " (map (fn [[k v]]
                              (str (print-form k) " " (print-form v)))
                            form))
         "}")

    ;; set
    (set? form)
    (str "#{" (str/join " " (map print-form form)) "}")

    ;; symbol — escape begin/end and word infix operators to avoid delimiter conflict
    (symbol? form)
    (let [n (name form)]
      (if (and (nil? (namespace form))
               (#{"begin" "end" "or" "and" "not=" "mod" "rem"} n))
        (str "/" n "/")
        (str form)))

    ;; keyword
    (keyword? form)
    (if (namespace form)
      (str ":" (namespace form) "/" (name form))
      (str ":" (name form)))

    ;; string
    (string? form) (pr-str form)

    ;; regex — escape bare quotes in the pattern.
    ;; Match escape sequences (\.) atomically so \\" is parsed as
    ;; (escaped-backslash)(bare-quote), not (backslash)(escaped-quote).
    (instance? #?(:clj java.util.regex.Pattern :cljs js/RegExp) form)
    (let [raw #?(:clj (.pattern ^java.util.regex.Pattern form) :cljs (.-source form))]
      (str "#\"" (str/replace raw #"\\.|\"" (fn [m] (if (= m "\"") "\\\"" m))) "\""))

    ;; char (JVM/Babashka only — ClojureScript has no char type)
    #?@(:clj [(char? form)
              (let [named {(char 10) "newline" (char 13) "return" (char 9) "tab"
                           (char 32) "space" (char 8) "backspace" (char 12) "formfeed"}]
                (if-let [n (get named form)]
                  (str \\ n)
                  (str \\ form)))])

    ;; number — preserve BigDecimal M and BigInt N suffixes, symbolic values
    #?@(:clj [(decimal? form) (str form "M")
              (instance? clojure.lang.BigInt form) (str form "N")
              (instance? java.math.BigInteger form) (str form "N")])
    (and (number? form)
         #?(:clj (Double/isNaN (double form))
            :cljs (js/isNaN form)))
    "##NaN"
    (and (number? form)
         #?(:clj (Double/isInfinite (double form))
            :cljs (and (not (js/isFinite form)) (not (js/isNaN form)))))
    (if (pos? (double form)) "##Inf" "##-Inf")
    (number? form) (str form)

    ;; tagged literal (JVM only — resolved at read time in ClojureScript)
    #?@(:clj [(tagged-literal? form)
              (str "#" (.-tag form) " " (print-form (.-form form)))

              ;; reader conditional — opaque passthrough
              (reader-conditional? form)
              (pr-str form)])

    ;; fallback
    :else (pr-str form)))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn print-beme-string
  "Print Clojure forms as beme text."
  [forms]
  (str/join "\n\n" (map print-form forms)))
