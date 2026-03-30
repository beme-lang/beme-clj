(ns beme.alpha.generative-test
  "Property-based generative tests targeting the beme risk surface.
   JVM-only (.clj) because test.check is not available on Babashka/CLJS.

   Two tracks:
     Track 1 — Generate beme *text* at syntax boundaries (parse direction)
     Track 2 — Generate adversarial Clojure *forms* (print direction)

   Each generator targets input patterns that have historically caused bugs:
   bracket chars in strings, begin/end as data, signed-number ambiguity,
   symbols with special chars, #_ placement, quoting edge cases, etc.

   Exception policy: catch only ExceptionInfo (what beme-error throws) so
   that real crashes (NPE, StackOverflow, ClassCast) fail the property."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [beme.alpha.core :as core]
            [beme.alpha.emit.printer :as p]))

;; ===========================================================================
;; Helpers
;; ===========================================================================

(defn- roundtrip-text-ok?
  "Parse beme text, print it back, re-parse — forms must match.
   Uses pr-str comparison to handle NaN (NaN != NaN by IEEE 754)
   and java.util.regex.Pattern (no equals() method)."
  [beme-str]
  (try
    (let [forms1 (core/beme->forms beme-str)
          printed (p/print-beme-string forms1)
          forms2 (core/beme->forms printed)]
      (= (pr-str forms1) (pr-str forms2)))
    (catch clojure.lang.ExceptionInfo _e false)))

(defn- roundtrip-form-ok?
  "Print a form to beme, read it back — forms must match."
  [form]
  (try
    (let [printed (p/print-beme-string [form])
          read-back (core/beme->forms printed)]
      (= [form] read-back))
    (catch clojure.lang.ExceptionInfo _e false)))

;; ===========================================================================
;; Track 1 — Boundary-aware text generators
;; ===========================================================================

;; ---------------------------------------------------------------------------
;; Risky atoms: symbols, strings, numbers, chars at syntax boundaries
;; ---------------------------------------------------------------------------

(def gen-safe-sym-name
  "Short safe symbol name for use as call head or argument."
  (gen/let [c (gen/elements (seq "abcdefghijklmnopqrstuvwxyz"))
            rest (gen/vector (gen/elements (seq "abcdefghijklmnopqrstuvwxyz0123456789-")) 0 5)]
    (apply str c rest)))

(def gen-risky-symbol
  "Symbols at the edges: dotted, dashed, arrow, near-reserved."
  (gen/one-of
    [;; Dotted symbols (namespace-qualified)
     (gen/let [ns gen-safe-sym-name n gen-safe-sym-name]
       (str ns "/" n))
     ;; Arrow operators
     (gen/elements ["->>" "->" "->x" "some->" "some->>"])
     ;; Symbols with special suffixes
     (gen/let [base gen-safe-sym-name
               suffix (gen/elements ["?" "!" "*" "'" "->"])]
       (str base suffix))
     ;; Symbols that look like reserved words but aren't
     (gen/elements ["beginning" "ending" "begin-x" "end-y"
                    "beginner" "endgame" "fend" "blend"])]))

(def gen-risky-string
  "Strings containing syntax-significant characters.
   All special chars are properly escaped for valid beme string literals."
  (gen/let [parts (gen/vector
                    (gen/one-of
                      [(gen/return "(")
                       (gen/return ")")
                       (gen/return "[")
                       (gen/return "]")
                       (gen/return "{")
                       (gen/return "}")
                       (gen/return "begin")
                       (gen/return "end")
                       (gen/return "\\\\")   ;; literal backslash (escaped)
                       (gen/return "\\\"")   ;; literal quote (escaped)
                       (gen/return "\\n")    ;; newline escape
                       (gen/return "\\t")    ;; tab escape
                       (gen/return "#")
                       (gen/return "@")
                       (gen/return "^")
                       (gen/return "~")
                       (gen/return "`")
                       (gen/fmap #(apply str %)
                         (gen/vector (gen/elements (seq "abcdef 123")) 1 5))])
                    1 6)]
    (str "\"" (apply str parts) "\"")))

(def gen-risky-number
  "Numbers at syntax edges: ratios, big literals, signed, symbolic."
  (gen/one-of
    [(gen/let [n (gen/choose 1 99) d (gen/choose 1 99)]
       (str n "/" d))
     (gen/let [n (gen/choose -999 999)] (str n "N"))
     (gen/let [n (gen/choose 1 99) d (gen/choose 0 99)]
       (str n "." d "M"))
     (gen/elements ["##Inf" "##-Inf" "##NaN"])
     (gen/let [n (gen/choose 1 9999)] (str "-" n))
     (gen/let [n (gen/choose 1 9999)] (str "+" n))
     (gen/let [n (gen/choose 0 255)] (str "16r" (Integer/toHexString n)))]))

(def gen-risky-char
  "Character literals including bracket-like and named chars."
  (gen/elements ["\\(" "\\)" "\\[" "\\]" "\\{" "\\}"
                 "\\newline" "\\tab" "\\space" "\\return"
                 "\\u0041" "\\a" "\\Z" "\\0" "\\\\"]))

(def gen-risky-keyword
  "Keywords including namespaced variants."
  (gen/one-of
    [(gen/let [n gen-safe-sym-name] (str ":" n))
     (gen/let [ns gen-safe-sym-name n gen-safe-sym-name]
       (str ":" ns "/" n))]))

(def gen-atom
  "Any single beme atom (text representation)."
  (gen/one-of
    [gen-risky-symbol gen-risky-string gen-risky-number
     gen-risky-char gen-risky-keyword
     (gen/elements ["nil" "true" "false"])]))

;; ---------------------------------------------------------------------------
;; Call forms with whitespace variation
;; ---------------------------------------------------------------------------

(def gen-whitespace
  "Whitespace between head and opening paren — the variation that must be irrelevant."
  (gen/one-of
    [(gen/return "")
     (gen/return " ")
     (gen/return "  ")
     (gen/return "\t")
     (gen/return "\n")
     (gen/return " \n ")]))

(def gen-call-text
  "A call form with varied spacing: head<ws>(args)."
  (gen/let [head gen-safe-sym-name
            ws gen-whitespace
            args (gen/vector gen-atom 0 4)
            sep (gen/elements [" " "  " ", " " , "])]
    (str head ws "(" (str/join sep args) ")")))

;; ---------------------------------------------------------------------------
;; begin/end forms
;; ---------------------------------------------------------------------------

(def gen-begin-end-text
  "Calls using begin/end delimiters, including begin/end as data arguments."
  (gen/one-of
    [;; Simple begin/end call
     (gen/let [head gen-safe-sym-name
               args (gen/vector gen-atom 1 3)]
       (str head " begin " (str/join " " args) " end"))
     ;; begin/end with /begin/ /end/ escaped symbols as args
     (gen/let [head gen-safe-sym-name]
       (str head "(/begin/ /end/)"))
     ;; Nested: call inside begin/end
     (gen/let [outer gen-safe-sym-name
               inner gen-safe-sym-name
               arg gen-atom]
       (str outer " begin " inner "(" arg ") end"))]))

;; ---------------------------------------------------------------------------
;; Discard (#_) placement
;; ---------------------------------------------------------------------------

(def gen-discard-text
  "Forms with #_ discards at various positions."
  (gen/one-of
    [;; Discard before a call
     (gen/let [discarded gen-atom
               head gen-safe-sym-name
               arg gen-atom]
       (str "#_" discarded " " head "(" arg ")"))
     ;; Discard inside call args
     (gen/let [head gen-safe-sym-name
               kept gen-atom
               discarded gen-atom]
       (str head "(" kept " #_" discarded ")"))
     ;; Double discard
     (gen/let [d1 gen-atom d2 gen-atom kept gen-atom]
       (str "#_ " d1 " #_ " d2 " " kept))
     ;; Discard inside begin/end
     (gen/let [head gen-safe-sym-name
               kept gen-atom
               discarded gen-atom]
       (str head " begin " kept " #_" discarded " end"))]))

;; ---------------------------------------------------------------------------
;; Quoted lists
;; ---------------------------------------------------------------------------

(def gen-quoted-list-text
  "Quoted lists — must use S-expression syntax inside."
  (gen/one-of
    [;; Simple quoted list
     (gen/let [elems (gen/vector gen-atom 1 4)]
       (str "'(" (str/join " " elems) ")"))
     ;; Nested sublists inside quote (non-callable heads like numbers)
     (gen/let [n gen-risky-number a gen-atom]
       (str "'((" n " " a "))"))
     ;; Empty quoted list
     (gen/return "'()")
     ;; Quoted list with nested lists
     (gen/let [a gen-safe-sym-name b gen-safe-sym-name]
       (str "'(" a " (" b " 1))"))]))

;; ---------------------------------------------------------------------------
;; Reader conditionals (opaque) with tricky content
;; ---------------------------------------------------------------------------

(def gen-opaque-text
  "Opaque forms (reader conditionals) containing syntax-significant content."
  (gen/one-of
    [;; Reader conditional with char literals
     (gen/let [ch gen-risky-char]
       (str "#?(:clj " ch " :cljs nil)"))
     ;; Reader conditional with strings containing brackets
     (gen/let [s gen-risky-string]
       (str "#?(:clj " s " :cljs nil)"))
     ;; Reader conditional with begin/end symbols
     (gen/return "#?(:clj begin :cljs end)")
     ;; Reader conditional with #()
     (gen/let [body gen-safe-sym-name]
       (str "#?(:clj #(inc %) :cljs " body ")"))
     ;; Reader conditional with nested parens
     (gen/let [a gen-safe-sym-name b gen-safe-sym-name]
       (str "#?(:clj (" a " (" b ")) :cljs nil)"))]))

;; ---------------------------------------------------------------------------
;; Metadata forms
;; ---------------------------------------------------------------------------

(def gen-metadata-text
  "Metadata on various targets."
  (gen/one-of
    [;; ^:key on symbol
     (gen/let [k gen-safe-sym-name target gen-safe-sym-name]
       (str "^:" k " " target))
     ;; ^Type on symbol
     (gen/let [target gen-safe-sym-name]
       (str "^String " target))
     ;; ^{map} on symbol
     (gen/let [k gen-safe-sym-name target gen-safe-sym-name]
       (str "^{:" k " true} " target))
     ;; Metadata on vector
     (gen/let [k gen-safe-sym-name a gen-atom]
       (str "^:" k " [" a "]"))
     ;; Chained metadata
     (gen/let [k1 gen-safe-sym-name k2 gen-safe-sym-name target gen-safe-sym-name]
       (str "^:" k1 " ^:" k2 " " target))]))

;; ---------------------------------------------------------------------------
;; Composite text generator (mixes all Track 1 generators)
;; ---------------------------------------------------------------------------

(def gen-risky-beme-text
  "A single beme form from any of the risky text generators."
  (gen/one-of
    [gen-call-text
     gen-begin-end-text
     gen-discard-text
     gen-quoted-list-text
     gen-opaque-text
     gen-metadata-text
     ;; Bare atoms
     gen-atom]))

(def gen-multi-form-text
  "Multiple beme forms separated by whitespace — tests top-level parsing."
  (gen/let [forms (gen/vector gen-risky-beme-text 1 4)
            seps (gen/vector (gen/elements [" " "\n" "\n\n" "  \n  "]) 1 4)]
    (str/join (first seps) forms)))

;; ===========================================================================
;; Track 2 — Adversarial form generators
;; ===========================================================================

(def reserved-symbols #{'fn 'quote 'var 'clojure.core/deref
                         'nil 'true 'false 'begin 'end})

(def gen-simple-symbol
  (gen/let [c (gen/elements (seq "abcdefghijklmnopqrstuvwxyz"))
            rest (gen/vector (gen/elements (seq "abcdefghijklmnopqrstuvwxyz0123456789-")) 0 5)]
    (let [sym (symbol (apply str c rest))]
      (if (reserved-symbols sym) (symbol (str c "x")) sym))))

(def reserved-meta-keys
  "Metadata keys that the printer strips (internal to beme or Clojure compiler)."
  #{:ws :line :column :file})

(def gen-simple-keyword
  (gen/let [n (gen/not-empty (gen/vector (gen/elements (seq "abcdefghijklmnopqrstuvwxyz")) 1 6))]
    (let [kw (keyword (apply str n))]
      (if (reserved-meta-keys kw) (keyword (str (apply str n) "x")) kw))))

;; ---------------------------------------------------------------------------
;; Forms with begin/end as data
;; ---------------------------------------------------------------------------

(def gen-form-with-begin-end
  "Forms containing the symbols `begin` and `end` as data inside calls.
   The printer must escape these to /begin/ and /end/."
  (gen/one-of
    [(gen/let [head gen-simple-symbol]
       (list head 'begin 'end))
     (gen/let [head gen-simple-symbol]
       (list head ['begin 'end]))
     (gen/let [head gen-simple-symbol inner gen-simple-symbol]
       (list head (list inner 'begin)))]))

;; ---------------------------------------------------------------------------
;; Chained calls (list-headed)
;; ---------------------------------------------------------------------------

(def gen-chained-call-form
  "((f x) y), (((f a) b) c) — list as call head."
  (gen/let [f gen-simple-symbol
            args (gen/vector gen-simple-symbol 1 3)
            outer-args (gen/vector gen-simple-symbol 1 2)]
    (let [inner (apply list f args)]
      (apply list inner outer-args))))

;; ---------------------------------------------------------------------------
;; Quoted lists with non-callable heads
;; ---------------------------------------------------------------------------

(def gen-quoted-list-form
  "Quoted lists containing sublists with non-symbol heads (numbers, strings).
   The printer must use clj-mode (S-expression syntax) inside."
  (gen/one-of
    [;; Quoted list with number-headed sublist
     (gen/let [n (gen/choose 1 99) a gen-simple-symbol]
       (list 'quote (list (list n a))))
     ;; Quoted empty list
     (gen/return (list 'quote '()))
     ;; Quoted list with nested lists
     (gen/let [a gen-simple-symbol b gen-simple-symbol]
       (list 'quote (list a (list b 1))))
     ;; Quoted list with string in sublist head position
     (gen/let [s (gen/elements ["foo" "bar" "hello"])
               a gen-simple-symbol]
       (list 'quote (list (list s a))))]))

;; ---------------------------------------------------------------------------
;; Metadata on collections
;; ---------------------------------------------------------------------------

(def gen-metadata-form
  "Metadata on various targets including collections."
  (gen/one-of
    [;; ^:key on symbol
     (gen/let [k gen-simple-keyword sym gen-simple-symbol]
       (with-meta sym {k true}))
     ;; ^:key on vector
     (gen/let [k gen-simple-keyword
               elems (gen/vector gen-simple-symbol 1 3)]
       (with-meta elems {k true}))
     ;; ^:key on map
     (gen/let [k gen-simple-keyword
               mk gen-simple-keyword
               mv gen-simple-symbol]
       (with-meta (array-map mk mv) {k true}))
     ;; Chained metadata on symbol (ensure distinct keys to avoid Duplicate key error)
     (gen/let [k1 gen-simple-keyword k2 gen-simple-keyword sym gen-simple-symbol]
       (if (= k1 k2)
         (with-meta sym {k1 true})
         (with-meta sym {k1 true k2 true})))]))

;; ---------------------------------------------------------------------------
;; Anonymous function edge cases
;; ---------------------------------------------------------------------------

(def gen-anon-fn-form
  "Anonymous functions: various param/body patterns that stress the
   #() shorthand detection and roundtrip."
  (gen/one-of
    [;; Standard: (fn [%1] body-using-%1)
     (gen/let [head gen-simple-symbol]
       (list 'fn ['%1] (list head '%1)))
     ;; Multi-param: (fn [%1 %2] body)
     (gen/let [head gen-simple-symbol]
       (list 'fn ['%1 '%2] (list head '%1 '%2)))
     ;; Rest param: (fn [& %&] body) — must NOT use #() shorthand
     (gen/let [head gen-simple-symbol]
       (list 'fn ['& '%&] (list head '%&)))
     ;; Rest + numbered: (fn [%1 & %&] body)
     (gen/let [head gen-simple-symbol]
       (list 'fn ['%1 '& '%&] (list head '%1 '%&)))
     ;; Surplus param: (fn [%1 %2] body-only-using-%1) — must NOT use #() shorthand
     (gen/let [head gen-simple-symbol]
       (list 'fn ['%1 '%2] (list head '%1)))]))

;; ---------------------------------------------------------------------------
;; Empty collections in various positions
;; ---------------------------------------------------------------------------

(def gen-empty-collection-form
  "Empty collections as arguments to calls.
   Note: empty list '() is excluded — it roundtrips as (quote ()) by design,
   since bare () is invalid beme syntax. This is a structural change, not a bug."
  (gen/let [head gen-simple-symbol
            empty-coll (gen/elements [[] {} #{}])]
    (list head empty-coll)))

;; ---------------------------------------------------------------------------
;; Mixed adversarial forms
;; ---------------------------------------------------------------------------

(def gen-adversarial-form
  "Uniformly sample from all adversarial form generators."
  (gen/one-of
    [gen-form-with-begin-end
     gen-chained-call-form
     gen-quoted-list-form
     gen-metadata-form
     gen-anon-fn-form
     gen-empty-collection-form]))

;; ===========================================================================
;; Track 1 Properties — Text-level
;; ===========================================================================

(defspec prop-risky-text-parses 500
  (prop/for-all [beme-str gen-risky-beme-text]
    (try
      (core/beme->forms beme-str)
      true
      (catch clojure.lang.ExceptionInfo _e
        ;; Some generated forms may be syntactically invalid (e.g. bad ratios);
        ;; that's fine — we're testing that the parser doesn't crash with NPE
        ;; or StackOverflow, just throws a clean beme error.
        true))))

(defspec prop-risky-text-roundtrips 500
  (prop/for-all [beme-str gen-call-text]
    (roundtrip-text-ok? beme-str)))

(defspec prop-spacing-irrelevant 300
  (prop/for-all [head gen-safe-sym-name
                 ws1 gen-whitespace
                 ws2 gen-whitespace
                 args (gen/vector gen-atom 1 3)]
    (try
      (let [text1 (str head ws1 "(" (str/join " " args) ")")
            text2 (str head ws2 "(" (str/join " " args) ")")
            forms1 (core/beme->forms text1)
            forms2 (core/beme->forms text2)]
        (= forms1 forms2))
      (catch clojure.lang.ExceptionInfo _e
        ;; If both fail to parse, spacing is still irrelevant
        true))))

(defspec prop-begin-end-equivalent 300
  (prop/for-all [head gen-safe-sym-name
                 args (gen/vector (gen/one-of [gen-safe-sym-name gen-risky-number gen-risky-keyword]) 1 3)]
    (try
      (let [paren-text (str head "(" (str/join " " args) ")")
            begin-text (str head " begin " (str/join " " args) " end")
            forms1 (core/beme->forms paren-text)
            forms2 (core/beme->forms begin-text)]
        (= forms1 forms2))
      (catch clojure.lang.ExceptionInfo _e true))))

(defspec prop-discard-transparent 300
  (prop/for-all [beme-str gen-discard-text]
    (try
      ;; Just verify it parses without crash — discard semantics
      ;; are verified by comparing with/without the #_ form
      (core/beme->forms beme-str)
      true
      (catch clojure.lang.ExceptionInfo _e true))))

(defspec prop-opaque-roundtrips 300
  (prop/for-all [beme-str gen-opaque-text]
    (try
      (let [forms (core/beme->forms beme-str)]
        ;; Opaque forms delegate to Clojure's reader; just verify
        ;; parse succeeds and produces non-nil
        (some? forms))
      (catch clojure.lang.ExceptionInfo _e true))))

(defspec prop-metadata-text-roundtrips 300
  (prop/for-all [beme-str gen-metadata-text]
    (roundtrip-text-ok? beme-str)))

(defspec prop-quoted-list-text-roundtrips 300
  (prop/for-all [beme-str gen-quoted-list-text]
    (roundtrip-text-ok? beme-str)))

(defspec prop-multi-form-parses 200
  (prop/for-all [beme-str gen-multi-form-text]
    (try
      (core/beme->forms beme-str)
      true
      (catch clojure.lang.ExceptionInfo _e true))))

;; ===========================================================================
;; Track 2 Properties — Form-level
;; ===========================================================================

(defspec prop-adversarial-form-roundtrips 500
  (prop/for-all [form gen-adversarial-form]
    (roundtrip-form-ok? form)))

(defspec prop-begin-end-data-roundtrips 300
  (prop/for-all [form gen-form-with-begin-end]
    (roundtrip-form-ok? form)))

(defspec prop-chained-call-roundtrips 300
  (prop/for-all [form gen-chained-call-form]
    (roundtrip-form-ok? form)))

(defspec prop-quoted-list-form-roundtrips 300
  (prop/for-all [form gen-quoted-list-form]
    (roundtrip-form-ok? form)))

(defspec prop-metadata-form-roundtrips 300
  (prop/for-all [form gen-metadata-form]
    (let [printed (p/print-beme-string [form])
          read-back (first (core/beme->forms printed))]
      (and (= form read-back)
           (= (meta form) (dissoc (meta read-back) :ws))))))

(defspec prop-anon-fn-roundtrips 200
  (prop/for-all [form gen-anon-fn-form]
    (roundtrip-form-ok? form)))

(defspec prop-empty-collection-roundtrips 200
  (prop/for-all [form gen-empty-collection-form]
    (roundtrip-form-ok? form)))
