(ns beme.alpha.scan.source
  "Scanner-level source-position utilities.
   Defines the character-level line/col model used by the tokenizer and grouper.
   Only \\n advances the line counter — \\r is a regular character that occupies
   a column. This matches sadvance! in the tokenizer.

   Note: this is the *scanner* line model, not a universal line definition.
   The error display module (beme.alpha.errors/source-context) uses
   str/split-lines which has different line-ending semantics (splits on
   both \\n and \\r\\n). The two models agree for LF sources but diverge
   for CRLF. See format-error for how the bridge is handled.")

;; ---------------------------------------------------------------------------
;; Character code utilities (shared by tokenizer and reader)
;; ---------------------------------------------------------------------------

(defn char-code
  "Platform-portable integer code point of a character."
  [ch]
  #?(:clj (int ch) :cljs (.charCodeAt ch 0)))

(def code-0 (char-code \0))
(def code-7 (char-code \7))
(def code-9 (char-code \9))
(def code-A (char-code \A))
(def code-F (char-code \F))
(def code-Z (char-code \Z))
(def code-a (char-code \a))
(def code-f (char-code \f))
(def code-z (char-code \z))

;; ---------------------------------------------------------------------------
;; Source position
;; ---------------------------------------------------------------------------

(defn line-col->offset
  "Convert 1-indexed line/col to a 0-indexed character offset in source.
   Uses the scanner line model: only \\n is a line break, \\r occupies a column.
   Returns (count source) if the target position is past the end of source.
   Callers that use the result for slicing should verify the offset is in bounds."
  [source line col]
  (let [n (count source)]
    (loop [i 0 cur-line 1 cur-col 1]
      (cond
        (and (= cur-line line) (= cur-col col)) i
        (>= i n) i
        (= (nth source i) \newline) (recur (inc i) (inc cur-line) 1)
        :else (recur (inc i) cur-line (inc cur-col))))))
