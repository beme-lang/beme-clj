(ns beme.alpha.parse.reader
  "beme reader: recursive-descent parser.
   Transforms beme tokens into Clojure forms."
  (:require [clojure.string :as str]
            [beme.alpha.errors :as errors]
            [beme.alpha.parse.resolve :as resolve]
            [beme.alpha.scan.source :as source]))

;; Sentinel for #_ discard. Contract:
;; - Returned by `parse-form-base` (via `:discard`) when the parsed form was a #_ discard
;; - `parse-form` passes it through (skips `parse-path-chain` for sentinels)
;; - Use `discard-sentinel?` to check — never use `identical?` directly
;; - MUST be filtered by every caller of `parse-form` that collects forms:
;;   1. `parse-forms-until` — filters in its accumulation loop
;;   2. `read-beme-string-from-tokens` — filters in its top-level loop
;;   3. Any new callsite of `parse-form` must handle this sentinel
;; - The `:open-anon-fn` handler rejects it (single-expression body cannot be discarded)
(def ^:private discard-sentinel #?(:clj (Object.) :cljs #js {}))

(defn- discard-sentinel?
  "Returns true if v is the discard sentinel. Use this instead of
   (identical? discard-sentinel v) to keep the contract grep-able."
  [v]
  (identical? discard-sentinel v))

;; ---------------------------------------------------------------------------
;; Parser state
;; ---------------------------------------------------------------------------

(def ^:private ^:const max-depth 200)

(defn- make-parser
  ([tokens] (make-parser tokens nil nil))
  ([tokens opts] (make-parser tokens opts nil))
  ([tokens opts source]
   {:tokens tokens :pos (volatile! 0) :depth (volatile! 0)
    :opts opts :clj-mode (volatile! false) :in-call-args (volatile! false)
    :colon-consumed (volatile! false) :no-cross-line-paren (volatile! false)
    :source source}))

(defn- peof? [{:keys [tokens pos]}]
  (>= @pos (count tokens)))

(defn- ppeek
  ([p] (ppeek p 0))
  ([{:keys [tokens pos]} offset]
   (let [i (+ @pos offset)]
     (when (< i (count tokens)) (nth tokens i)))))

(defn- padvance! [{:keys [pos]}] (vswap! pos inc))

(defn- plast-loc
  "Location of the last consumed token, or {} if none."
  [{:keys [tokens pos]}]
  (let [i (dec @pos)]
    (if (and (>= i 0) (< i (count tokens)))
      (select-keys (nth tokens i) [:line :col])
      {})))

(defn- error-data
  "Merge source into error data map so beme-error can attach :source-context."
  [p data]
  (cond-> data
    (:source p) (assoc :source (:source p))))

(def ^:private token-name
  "Human-readable names for token types."
  {:close-paren  ")"
   :close-bracket "]"
   :close-brace  "}"
   :open-paren   "("
   :open-bracket "["
   :open-brace   "{"
   :open-set     "#{"
   :open-anon-fn "#("
   :symbol       "symbol"
   :escaped-symbol "symbol"
   :keyword      "keyword"
   :number       "number"
   :string       "string"
   :char         "character"
   :regex        "regex"
   :deref        "@"
   :meta         "^"
   :quote        "'"
   :unquote      "~"
   :unquote-splicing "~@"
   :var-quote    "#'"
   :discard      "#_"
   :tagged-literal "tagged literal"
   :reader-cond-raw "reader conditional"
   :reader-cond-start "reader conditional"
   :namespaced-map-raw "namespaced map"
   :namespaced-map-start "namespaced map prefix"
   :syntax-quote-raw "syntax-quote"
   :syntax-quote-start "syntax-quote"
   :close-end    "end"})

(def ^:private closer-name
  "Human-readable descriptions for closing delimiters."
  {:close-paren  "closing )"
   :close-bracket "closing ]"
   :close-brace  "closing }"
   :close-end    "end"})

(def ^:private closer-context
  "What structure each closer terminates."
  {:close-paren  "call"
   :close-bracket "vector"
   :close-brace  "map/set"
   :close-end    "call"})

(defn- begin-symbol? [tok]
  (and (= :symbol (:type tok)) (= "begin" (:value tok))))

(defn- end-symbol? [tok]
  (and (= :symbol (:type tok)) (= "end" (:value tok))))

(defn- describe-token [tok]
  (let [typ (:type tok)
        n (get token-name typ (name typ))]
    (if (#{:symbol :keyword :number :string :char :regex} typ)
      (str n " " (:value tok))
      n)))

(defn- tok-type? [tok typ]
  (and tok (= (:type tok) typ)))

(declare parse-form parse-form-base parse-expr parse-vector parse-map parse-call-args call-opener?)

;; ---------------------------------------------------------------------------
;; Collections
;; ---------------------------------------------------------------------------

(defn- parse-forms-until
  ([p end-type] (parse-forms-until p end-type nil))
  ([p end-type open-loc]
   (let [end-pred (if (= end-type :close-end)
                    end-symbol?
                    #(= end-type (:type %)))]
     (loop [forms []]
       (when (peof? p)
         (let [ctx (get closer-context end-type "expression")
               closer (get closer-name end-type (name end-type))]
           (errors/beme-error
             (str "Unclosed " ctx " — expected " closer " but reached end of input")
             (error-data p (cond-> (assoc (plast-loc p) :incomplete true)
                             open-loc (assoc :secondary [{:line (:line open-loc) :col (:col open-loc) :label "opened here"}])
                             open-loc (assoc :hint (str "Add " (get token-name end-type (name end-type)) " to close this " ctx)))))))
       (let [tok (ppeek p)]
         (if (end-pred tok)
           (do (padvance! p) forms)
           (let [form (parse-expr p)]
             (if (discard-sentinel? form)
               (recur forms)
               (recur (conj forms form))))))))))

;; ---------------------------------------------------------------------------
;; Surface syntax: infix operators (Pratt parser)
;; ---------------------------------------------------------------------------

(def ^:private infix-prec
  "Operator precedence for surface-syntax infix. Higher = tighter binding.
   All operators are left-associative."
  {"|>" 5, ".>" 5
   "or"  10
   "and" 20
   "="   30, "not=" 30, "<" 30, ">" 30, "<=" 30, ">=" 30
   "+"   40, "-"   40
   "*"   50, "/"   50, "mod" 50, "rem" 50})

(defn- infix-tok?
  "Is this token a surface-syntax infix operator?"
  [tok]
  (and tok (= :symbol (:type tok)) (contains? infix-prec (:value tok))))

(defn- pratt-climb
  "Pratt precedence-climbing loop. Consumes infix operators while their
   precedence >= min-prec, building left-associative binary forms.
   No-ops in clj-mode (inside quoted lists).
   An operator is NOT treated as infix if followed by '(' or 'begin',
   since that means M-expression call style: +(1 2) not a + b."
  [p left min-prec]
  (if @(:clj-mode p)
    left
    (loop [left left]
      (let [tok  (ppeek p)
            prec (when (infix-tok? tok) (get infix-prec (:value tok)))]
        (if (and prec (>= prec min-prec)
                 ;; Don't treat as infix if op is immediately followed by ( with no space
                 ;; (that's M-expression call style: +(a b) not a + b).
                 ;; Paren-grouping like a + (b - c) has a space, so offsets differ.
                 (not (and (call-opener? (ppeek p 1))
                           (some? (:end-offset tok))
                           (= (:offset (ppeek p 1)) (:end-offset tok)))))
          (let [op-str (:value tok)
                op (case op-str
                     "|>" '->>
                     ".>" '->
                     (symbol op-str))
                _  (padvance! p)
                right (parse-expr p (inc prec))]
            (recur (if (and (seq? left) (= (first left) op))
                     ;; Flatten consecutive same-op pipes: (->> xs f) |> g → (->> xs f g)
                     (apply list op (concat (rest left) [right]))
                     (list op left right))))
          left)))))

;; ---------------------------------------------------------------------------
;; Surface syntax: block forms
;; ---------------------------------------------------------------------------

(defn- colon-tok?
  "Bare ':' token — used as block-start delimiter."
  [tok]
  (and tok (= :keyword (:type tok)) (= ":" (:value tok))))

(defn- bind-op?
  "':=' assignment operator."
  [tok]
  (and tok (= :keyword (:type tok)) (= ":=" (:value tok))))

(defn- arrow-tok?
  "'=>' clause arrow in cond/case."
  [tok]
  (and tok (= :symbol (:type tok)) (= "=>" (:value tok))))

(defn- sym-value? [tok s]
  (and tok (= :symbol (:type tok)) (= s (:value tok))))

(defn- body-terminator?
  "Is this token a block body terminator?
   Handles both 'else' + ':' and 'else:' as a single token (tokenizer merges them)."
  [tok]
  (and tok (= :symbol (:type tok))
       (#{"end" "else" "else:" "catch" "catch:" "finally" "finally:"} (:value tok))))

(defn- closing-tok?
  "Is this token a closing delimiter (}, ), ], end)?
   Block forms should not trigger when a closing delimiter follows."
  [tok]
  (and tok (or (#{:close-paren :close-bracket :close-brace} (:type tok))
               (end-symbol? tok))))

(defn- consume-colon!
  "Consume a bare ':' block-start. Errors if not found.
   Also succeeds when :colon-consumed flag is set (tokenizer merged 'word:' into one token)."
  [p context]
  (cond
    @(:colon-consumed p)
    (vreset! (:colon-consumed p) false) ; colon was merged into preceding token

    (colon-tok? (ppeek p))
    (padvance! p)

    :else
    (errors/beme-error
      (str "Expected ':' to start " context " body")
      (error-data p (plast-loc p)))))

(defn- strip-trailing-colon
  "If form is a symbol with trailing ':', strip it and return [bare-sym true].
   Otherwise return [form false]. Handles the tokenizer merging 'word:' into one symbol."
  [form]
  (if (and (symbol? form) (str/ends-with? (name form) ":"))
    [(symbol (subs (name form) 0 (dec (count (name form))))) true]
    [form false]))

(defn- consume-block-intro!
  "Consume a block-intro keyword that may have been merged with ':' by the tokenizer.
   E.g. 'else:' is a single token; 'else' followed by ':' is two tokens.
   Advances past the keyword and, if bare keyword, consumes the following ':'."
  [p kw]
  (let [tok (ppeek p)
        val (:value tok)]
    (padvance! p) ; consume the keyword (possibly with trailing ':')
    (when-not (str/ends-with? val ":")
      ;; bare keyword — ':' must follow separately
      (consume-colon! p kw))))

(defn- consume-end!
  "Consume the 'end' keyword. Errors if not found."
  [p context]
  (if (end-symbol? (ppeek p))
    (padvance! p)
    (errors/beme-error
      (str "Expected 'end' to close " context " block")
      (error-data p (plast-loc p)))))

(def ^:private let-stmt-marker ::let-stmt)

(defn- wrap-body-lets
  "Convert [::let-stmt target val] markers in a body vector into nested
   (let [target val ...] ...) forms. Consecutive let-stmts are merged."
  [stmts]
  (loop [items (seq stmts) acc []]
    (if (nil? items)
      acc
      (let [item (first items)]
        (if (and (vector? item) (= let-stmt-marker (first item)))
          (let [[lets rest] (split-with #(and (vector? %) (= let-stmt-marker (first %))) items)
                bindings    (vec (mapcat (fn [s] [(nth s 1) (nth s 2)]) lets))
                remaining   (wrap-body-lets (vec rest))]
            (conj acc (apply list 'let bindings remaining)))
          (recur (next items) (conj acc item)))))))

(defn- parse-body
  "Parse a body: expressions until a block terminator (end/else/catch/finally).
   extra-terminator? is an optional predicate for additional stop conditions
   (e.g. open-paren to stop multi-arity defn bodies at the next arity).
   Returns a vector of forms with let-stmts wrapped."
  ([p] (parse-body p nil))
  ([p extra-terminator?]
   (let [stmts
         (loop [stmts []]
           (cond
             (peof? p)
             (errors/beme-error "Unexpected end of file — missing 'end'"
                                (error-data p (plast-loc p)))
             (body-terminator? (ppeek p))
             stmts
             (and extra-terminator? (extra-terminator? (ppeek p)))
             stmts
             :else
             (let [form (parse-expr p)]
               (if (discard-sentinel? form)
                 (recur stmts)
                (recur (conj stmts form))))))]
     (wrap-body-lets stmts))))

(defn- parse-binding-target
  "Parse a binding target: symbol, vector (destructure), or map (destructure)."
  [p]
  (let [tok (ppeek p)]
    (case (:type tok)
      :open-bracket (parse-vector p)
      :open-brace   (parse-map p)
      (parse-form p))))

(defn- parse-one-binding
  "Parse 'target := expr'. Returns [target expr]."
  [p]
  (let [target (parse-binding-target p)]
    (when-not (bind-op? (ppeek p))
      (errors/beme-error
        "Expected ':=' in binding"
        (error-data p (plast-loc p))))
    (padvance! p)  ;; consume ':='
    [target (parse-expr p)]))

(defn- another-binding?
  "Peek ahead: is the next token a binding target followed by ':='?
   Used to decide whether to parse more bindings after the first."
  [p]
  (let [tok (ppeek p)]
    (and tok
         (not (colon-tok? tok))
         (not (body-terminator? tok))
         (not (peof? p))
         (or (tok-type? tok :symbol) (tok-type? tok :open-bracket) (tok-type? tok :open-brace))
         (not (#{"end" "else" "catch" "finally" "in" "when" "while" "let"} (:value tok)))
         (bind-op? (ppeek p 1)))))

(defn- parse-bindings
  "Parse one or more comma-separated bindings: 'x := 1, y := 2'.
   Returns flat vector [x 1 y 2]."
  [p]
  (loop [binds (let [[t v] (parse-one-binding p)] [t v])]
    (if (another-binding? p)
      (let [[t v] (parse-one-binding p)]
        (recur (conj binds t v)))
      binds)))

;; --- surface-block-fn? ---
;; For fn/defn, detect whether next '(' is surface-style params followed by ':'
;; rather than M-expression call args. Scans forward to find matching ')' then
;; checks for ':'.
(defn- surface-block-fn?
  "Returns true when current position is '(' that belongs to surface-style
   fn/defn params: the matching ')' is followed by ':'."
  [p]
  (when (tok-type? (ppeek p) :open-paren)
    (let [tokens (:tokens p)
          start  @(:pos p)]
      (loop [i (inc start) depth 1]
        (let [tok (when (< i (count tokens)) (nth tokens i))]
          (cond
            (nil? tok) false
            (tok-type? tok :open-paren)  (recur (inc i) (inc depth))
            (tok-type? tok :close-paren)
            (if (= depth 1)
              (let [after (when (< (inc i) (count tokens)) (nth tokens (inc i)))]
                (colon-tok? after))
              (recur (inc i) (dec depth)))
            :else (recur (inc i) depth)))))))

;; --- Individual block parsers ---
;; Each is called AFTER the keyword symbol has been consumed.

(defn- parse-if-block
  "if condition: body [else: body] end"
  [p kw]
  (let [cond-raw              (parse-expr p)
        [cond-expr colon-in-cond?] (strip-trailing-colon cond-raw)]
    (when-not colon-in-cond? (consume-colon! p "if"))
    (let [then-body (parse-body p)
          peek-val  (:value (ppeek p))]
      (if (#{"else" "else:"} peek-val)
        (do (consume-block-intro! p "else")
            (let [else-body (parse-body p)]
              (consume-end! p "if")
              (apply list kw cond-expr (concat then-body else-body))))
        (do (consume-end! p "if")
            (apply list kw cond-expr then-body))))))

(defn- parse-when-block
  "when condition: body end"
  [p kw]
  (let [cond-raw              (parse-expr p)
        [cond-expr colon-in-cond?] (strip-trailing-colon cond-raw)]
    (when-not colon-in-cond? (consume-colon! p "when"))
    (let [body (parse-body p)]
      (consume-end! p "when")
      (apply list kw cond-expr body))))

(defn- parse-let-block
  "let x := val        — let-statement (no body, scopes to enclosing block)
   let x := val:       — let-expression with body and end"
  [p kw]
  (let [[target val] (parse-one-binding p)]
    (if (colon-tok? (ppeek p))
      ;; let-expression
      (do (padvance! p)
          (let [body (parse-body p)]
            (consume-end! p "let")
            (apply list kw [target val] body)))
      ;; let-statement marker — parse-body wraps these into let forms
      [let-stmt-marker target val])))

(defn- parse-loop-block
  "loop x := 0, y := n: body end"
  [p]
  (let [raw-binds (parse-bindings p)
        ;; Last binding value may have ':' merged (e.g. 'items:' instead of 'items' + ':')
        [last-val colon-in-last?] (strip-trailing-colon (last raw-binds))
        binds (if (and colon-in-last? (> (count raw-binds) 0))
                (conj (pop raw-binds) last-val)
                raw-binds)]
    (when-not colon-in-last? (consume-colon! p "loop"))
    (let [body (parse-body p)]
      (consume-end! p "loop")
      (apply list 'loop binds body))))

(defn- parse-do-block
  "do: body end"
  [p]
  (consume-colon! p "do")
  (let [body (parse-body p)]
    (consume-end! p "do")
    (apply list 'do body)))

(defn- parse-fn-params
  "Parse fn/defn params from '(x y z)'. Consumes the parens."
  [p]
  (vec (parse-call-args p)))

(defn- parse-fn-block
  "fn(params): body end  — anonymous function.
   Requires surface-block-fn? to be true (called from block-parsers dispatch)."
  [p]
  (let [params (parse-fn-params p)
        _      (consume-colon! p "fn")
        body   (parse-body p)]
    (consume-end! p "fn")
    (apply list 'fn params body)))

(defn- parse-defn-block
  "defn name(params): body end  — function definition."
  [p kw]
  (let [name-tok (ppeek p)
        _        (when-not (tok-type? name-tok :symbol)
                   (errors/beme-error "Expected function name after defn"
                                      (error-data p (plast-loc p))))
        _        (padvance! p)
        ;; name may have ':' merged for multi-arity: 'greet:' → name=greet, colon-in-name=true
        name-raw (:value name-tok)
        [fname-str colon-in-name?] (if (and (str/ends-with? name-raw ":") (> (count name-raw) 1))
                                     [(subs name-raw 0 (dec (count name-raw))) true]
                                     [name-raw false])
        fname    (symbol fname-str)]
    (if (or colon-in-name? (colon-tok? (ppeek p)))
      ;; Multi-arity: defn name: (p1): body (p2): body end
      (do (when-not colon-in-name? (padvance! p)) ; consume ':' if separate
          (let [arities
                (loop [arities []]
                  (cond
                    (peof? p)          (errors/beme-error "Unclosed defn" (error-data p (plast-loc p)))
                    (end-symbol? (ppeek p)) arities
                    (tok-type? (ppeek p) :open-paren)
                    (let [params (parse-fn-params p)
                          _      (consume-colon! p "defn arity")
                          ;; stop at '(' (next arity) or 'end'
                          ;; :no-cross-line-paren prevents f\n(next-arity-params) from chaining
                          _      (vreset! (:no-cross-line-paren p) true)
                          body   (parse-body p #(tok-type? % :open-paren))
                          _      (vreset! (:no-cross-line-paren p) false)]
                      (recur (conj arities (apply list params body))))
                    :else (errors/beme-error "Expected '(' for arity in multi-arity defn"
                                             (error-data p (select-keys (ppeek p) [:line :col])))))]
            (consume-end! p "defn")
            (apply list kw fname arities)))
      ;; Single-arity
      (let [params (parse-fn-params p)
            _      (consume-colon! p "defn")
            body   (parse-body p)]
        (consume-end! p "defn")
        (apply list kw fname params body)))))

(defn- parse-for-bindings
  "Parse 'x in xs, when pred, y in ys' for-loop bindings.
   Consumes the trailing ':' (separate token or merged into last value/modifier).
   Returns flat vector as Clojure for/doseq expects: [x xs :when pred y ys]."
  [p]
  (loop [binds []]
    (cond
      (colon-tok? (ppeek p))
      (do (padvance! p) binds) ; consume ':' and return
      (body-terminator? (ppeek p))
      binds
      ;; modifier: when/while/let
      (and (tok-type? (ppeek p) :symbol)
           (#{"when" "while" "let"} (:value (ppeek p))))
      (let [mod-tok (ppeek p)
            _       (padvance! p)
            mod-kw  (keyword (:value mod-tok))
            val-raw (parse-expr p)
            [val colon?] (strip-trailing-colon val-raw)]
        (if colon? (conj binds mod-kw val) (recur (conj binds mod-kw val))))
      ;; binding: target in source
      :else
      (let [target (parse-binding-target p)]
        (when-not (sym-value? (ppeek p) "in")
          (errors/beme-error "Expected 'in' in for binding"
                             (error-data p (plast-loc p))))
        (padvance! p)  ;; consume 'in'
        (let [source-raw (parse-expr p)
              [source colon?] (strip-trailing-colon source-raw)]
          (if colon? (conj binds target source) (recur (conj binds target source))))))))

(defn- parse-for-block
  "for x in xs, when pred: body end
   parse-for-bindings already consumes the trailing ':'."
  [p kw]
  (let [binds (parse-for-bindings p)
        body  (parse-body p)]
    (consume-end! p "for")
    (apply list kw binds body)))

(defn- parse-cond-block
  "cond: test => val ... [else => val] end"
  [p]
  (consume-colon! p "cond")
  (let [clauses
        (loop [clauses []]
          (cond
            (peof? p)
            (errors/beme-error "Unclosed cond" (error-data p (plast-loc p)))
            (end-symbol? (ppeek p))
            clauses
            :else
            (let [test (if (sym-value? (ppeek p) "else")
                         (do (padvance! p) :else)
                         (parse-expr p))]
              (when-not (arrow-tok? (ppeek p))
                (errors/beme-error "Expected '=>' in cond clause"
                                   (error-data p (plast-loc p))))
              (padvance! p)  ;; consume '=>'
              (let [val (parse-expr p)]
                (recur (conj clauses test val))))))]
    (consume-end! p "cond")
    (apply list 'cond clauses)))

(defn- parse-case-block
  "case expr: val => result ... [else => result] end"
  [p]
  (let [dispatch-raw (parse-expr p)
        [dispatch colon?] (strip-trailing-colon dispatch-raw)]
    (when-not colon? (consume-colon! p "case"))
    (let [clauses
          (loop [clauses [] else-val nil]
            (cond
              (peof? p)
              (errors/beme-error "Unclosed case" (error-data p (plast-loc p)))
              (end-symbol? (ppeek p))
              {:clauses clauses :else else-val}
              :else
              (let [test (if (sym-value? (ppeek p) "else")
                           (do (padvance! p) ::else)
                           (parse-expr p))]
                (when-not (arrow-tok? (ppeek p))
                  (errors/beme-error "Expected '=>' in case clause"
                                     (error-data p (plast-loc p))))
                (padvance! p)
                (let [val (parse-expr p)]
                  (if (= test ::else)
                    (recur clauses val)
                    (recur (conj clauses test val) else-val))))))]
      (consume-end! p "case")
      (let [{:keys [clauses] else-val :else} clauses]
        (apply list 'case dispatch
               (cond-> clauses else-val (conj else-val)))))))

(defn- parse-try-block
  "try: body catch ExType e: body [finally: body] end"
  [p]
  (consume-colon! p "try")
  (let [try-body (parse-body p)
        clauses
        (loop [clauses []]
          (cond
            (peof? p)
            (errors/beme-error "Unclosed try" (error-data p (plast-loc p)))
            (end-symbol? (ppeek p))
            clauses
            (#{"catch" "catch:"} (:value (ppeek p)))
            (do (padvance! p) ; consume 'catch' or 'catch:'
                (let [exc-type (parse-form p)
                      bind-raw (parse-form p)
                      ;; binding may have ':' merged: 'e:' → binding=e, colon consumed
                      [binding colon-in-binding?] (strip-trailing-colon bind-raw)]
                  (when-not colon-in-binding? (consume-colon! p "catch"))
                  (let [body (parse-body p)]
                    (recur (conj clauses (apply list 'catch exc-type binding body))))))
            (#{"finally" "finally:"} (:value (ppeek p)))
            (do (consume-block-intro! p "finally")
                (let [body (parse-body p)]
                  (recur (conj clauses (apply list 'finally body)))))
            :else
            (errors/beme-error "Expected 'catch', 'finally', or 'end' in try block"
                               (error-data p (select-keys (ppeek p) [:line :col])))))]
    (consume-end! p "try")
    (apply list 'try (concat try-body clauses))))

;; ---------------------------------------------------------------------------
;; Block-parsers registry
;; Each entry: symbol → (fn [p] ...) called AFTER the keyword is consumed
;; ---------------------------------------------------------------------------

(def ^:private block-parsers
  {'if       #(parse-if-block % 'if)
   'if-not   #(parse-if-block % 'if-not)
   'if-let   #(parse-if-block % 'if-let)
   'if-some  #(parse-if-block % 'if-some)
   'when     #(parse-when-block % 'when)
   'when-not #(parse-when-block % 'when-not)
   'when-let #(parse-when-block % 'when-let)
   'when-some #(parse-when-block % 'when-some)
   'let      #(parse-let-block % 'let)
   'binding  #(parse-let-block % 'binding)
   'with-open #(parse-let-block % 'with-open)
   'loop     (fn [p] (parse-loop-block p))
   'do       (fn [p] (parse-do-block p))
   'fn       (fn [p] (parse-fn-block p))
   'defn     #(parse-defn-block % 'defn)
   'defn-    #(parse-defn-block % 'defn-)
   'defmacro #(parse-defn-block % 'defmacro)
   'for      #(parse-for-block % 'for)
   'doseq    #(parse-for-block % 'doseq)
   'dotimes  #(parse-for-block % 'dotimes)
   'cond     (fn [p] (parse-cond-block p))
   'case     (fn [p] (parse-case-block p))
   'try      (fn [p] (parse-try-block p))})

(defn- parse-vector [p]
  (let [loc (select-keys (ppeek p) [:line :col])]
    (padvance! p) ; [
    (vec (parse-forms-until p :close-bracket loc))))

(defn- parse-map [p]
  (let [loc (select-keys (ppeek p) [:line :col])]
    (padvance! p) ; {
    (let [forms (parse-forms-until p :close-brace loc)]
      (when (odd? (count forms))
        (errors/beme-error (str "Map literal requires an even number of forms, but got " (count forms))
                           (error-data p (assoc loc :hint "Maps need key-value pairs — check for a missing key or value"))))
      (let [m (apply array-map forms)]
        (when (not= (count m) (/ (count forms) 2))
          (errors/beme-error "Duplicate key in map literal"
                             (error-data p loc)))
        m))))

(defn- parse-set [p]
  (let [loc (select-keys (ppeek p) [:line :col])]
    (padvance! p) ; #{
    (let [forms (parse-forms-until p :close-brace loc)
          s (set forms)]
      (when (not= (count s) (count forms))
        (errors/beme-error "Duplicate element in set literal"
                           (error-data p loc)))
      s)))

;; ---------------------------------------------------------------------------
;; Call: f(args...) or f begin args... end
;; ---------------------------------------------------------------------------

(defn- call-opener? [tok]
  (or (tok-type? tok :open-paren)
      (begin-symbol? tok)))

(defn- parse-call-args [p]
  (let [tok (ppeek p)
        begin? (begin-symbol? tok)
        closer-type (if begin? :close-end :close-paren)
        loc (select-keys tok [:line :col])
        prev-in-call-args @(:in-call-args p)]
    (padvance! p) ; ( or begin
    (vreset! (:in-call-args p) true)
    (let [result (parse-forms-until p closer-type loc)]
      (vreset! (:in-call-args p) prev-in-call-args)
      result)))


;; ---------------------------------------------------------------------------
;; #() anonymous function — % param helpers
;; ---------------------------------------------------------------------------

(defn- percent-param-type
  "If sym is a % parameter symbol, return its type: :bare, :rest, or the integer N."
  [sym]
  (when (symbol? sym)
    (let [n (name sym)]
      (cond
        (= n "%") :bare
        (= n "%&") :rest
        (and (str/starts-with? n "%")
             (> (count n) 1)
             (every? #(let [c (source/char-code %)] (and (>= c source/code-0) (<= c source/code-9))) (seq (subs n 1))))
        (#?(:clj Long/parseLong :cljs #(js/parseInt % 10)) (subs n 1))
        :else nil))))

(defn- find-percent-params
  "Walk form collecting % param types. Skips nested (fn ...) bodies."
  [form]
  (cond
    (symbol? form)
    (if-let [p (percent-param-type form)] #{p} #{})

    (and (seq? form) (= 'fn (first form)))
    #{} ; don't recurse into nested fn / inner #()

    (seq? form)
    (reduce into #{} (map find-percent-params form))

    (vector? form)
    (reduce into #{} (map find-percent-params form))

    (map? form)
    (reduce into #{} (mapcat (fn [[k v]] [(find-percent-params k) (find-percent-params v)]) form))

    (set? form)
    (reduce into #{} (map find-percent-params form))

    #?@(:clj [(tagged-literal? form)
              (find-percent-params (.-form form))])

    :else #{}))

(defn- normalize-bare-percent
  "Replace bare % with %1 in form. Skips nested (fn ...) bodies."
  [form]
  (cond
    (and (symbol? form) (= "%" (name form))) (symbol "%1")

    (and (seq? form) (= 'fn (first form)))
    form ; don't recurse into nested fn

    (seq? form)
    (apply list (map normalize-bare-percent form))

    (vector? form)
    (mapv normalize-bare-percent form)

    (map? form)
    (into {} (map (fn [[k v]] [(normalize-bare-percent k) (normalize-bare-percent v)]) form))

    (set? form)
    (set (map normalize-bare-percent form))

    #?@(:clj [(tagged-literal? form)
              (tagged-literal (.-tag form) (normalize-bare-percent (.-form form)))])

    :else form))

(defn- build-anon-fn-params
  "Build [%1 %2 ...] or [%1 & %&] param vector from collected param types."
  [param-set]
  (let [has-bare? (contains? param-set :bare)
        has-rest? (contains? param-set :rest)
        nums (filter number? param-set)
        max-n (if (seq nums) (apply max nums) (if has-bare? 1 0))]
    (cond-> (mapv #(symbol (str "%" %)) (range 1 (inc max-n)))
      has-rest? (into ['& (symbol "%&")]))))


;; ---------------------------------------------------------------------------
;; Main parse dispatch
;; ---------------------------------------------------------------------------

(defn- maybe-call
  "If next token is ( or begin, parse call args and wrap — spacing is irrelevant.
   In clj-mode (inside quoted lists), never forms calls — returns head as-is.
   When :no-cross-line-paren is set, cross-line ( does not form a call
   (used inside multi-arity defn arity bodies to prevent param-list bleed)."
  [p head]
  (if @(:clj-mode p)
    head
    (let [next-tok (ppeek p)]
      (if (call-opener? next-tok)
        (if (and @(:no-cross-line-paren p)
                 (tok-type? next-tok :open-paren)
                 (let [prev-tok (when (> @(:pos p) 0) (nth (:tokens p) (dec @(:pos p))))]
                   (and prev-tok (> (:line next-tok 0) (:end-line prev-tok 0)))))
          head
          (let [args (parse-call-args p)]
            (apply list head args)))
        head))))

(defn- parse-form-base
  "Parse a single beme form."
  [p]
  (let [tok (ppeek p)]
    (when-not tok
      (errors/beme-error "Unexpected end of input — expected a form" (error-data p (assoc (plast-loc p) :incomplete true))))
    (case (:type tok)
      (:symbol :escaped-symbol)
      (let [s (:value tok)]
        (padvance! p)
        (case s
          "nil" nil
          "true" true
          "false" false
          ;; surface block syntax: dispatch if keyword is registered AND
          ;; not in clj-mode.
          ;; Inside (call-args), suppress block parsers EXCEPT fn/fn* so that
          ;; M-expression style works: if(cond t f), let([x 1] body), etc.
          ;; fn/fn* are allowed in call-args: map(fn(x): x + 1 end, xs).
          ;; Handle tokenizer-merged 'keyword:' (e.g. 'do:' = 'do' + ':').
          ;; do/try/cond only trigger if ':' immediately follows (or was merged).
          (let [;; Handle merged colon: "do:" → base "do", colon-merged? = true
                colon-merged? (and (str/ends-with? s ":") (> (count s) 1)
                                   (not (= s ":")))
                s-base        (if colon-merged? (subs s 0 (dec (count s))) s)
                sym           (symbol s)
                sym-base      (symbol s-base)
                block-fn      (and (not @(:clj-mode p))
                                   (or (not @(:in-call-args p))
                                       ;; fn/fn* in call-args only when surface-block style
                                       (and ('#{fn fn*} sym-base) (surface-block-fn? p)))
                                   (get block-parsers sym-base))]
            (if block-fn
              (cond
                ;; do/try/cond only trigger as surface block when ':' follows or was merged
                (and (#{'do 'try 'cond} sym-base)
                     (not colon-merged?)
                     (not (colon-tok? (ppeek p))))
                (maybe-call p sym) ; plain symbol, not a block
                ;; closing delimiter follows — can't start a block
                (closing-tok? (ppeek p))
                (maybe-call p sym)
                ;; nil follows (EOF) — can't start a block
                (nil? (ppeek p))
                (maybe-call p sym)
                ;; fn/defn/etc: fall back to M-expr call if not surface params
                (and (call-opener? (ppeek p))
                     (not (surface-block-fn? p)))
                (maybe-call p sym)
                ;; surface block form — set colon-consumed flag if colon was merged
                :else
                (do (when colon-merged?
                      (vreset! (:colon-consumed p) true))
                    (block-fn p)))
              (maybe-call p sym)))))

      :keyword
      (let [v (:value tok)]
        (padvance! p)
        (if (str/starts-with? v "::")
          ;; Auto-resolve keywords — resolve at read time if resolver available,
          ;; otherwise defer to eval time via read-string wrapper
          (let [resolve-kw (:resolve-keyword (:opts p))]
            (maybe-call p (resolve/resolve-auto-keyword v (select-keys tok [:line :col]) resolve-kw)))
          (let [s (subs v 1)
                i (str/index-of s "/")]
            (maybe-call p (if (some? i)
                            (keyword (subs s 0 i) (subs s (inc i)))
                            (keyword s))))))

      :number
      (do (padvance! p)
          (resolve/resolve-number (:value tok) (select-keys tok [:line :col])))

      :string
      (do (padvance! p)
          (resolve/resolve-string (:value tok) (select-keys tok [:line :col])))

      :char
      (do (padvance! p)
          (resolve/resolve-char (:value tok) (select-keys tok [:line :col])))

      :regex
      (do (padvance! p)
          (resolve/resolve-regex (:value tok) (select-keys tok [:line :col])))

      :open-paren
      (if @(:clj-mode p)
        ;; In clj-mode (inside quoted lists), bare parens create lists
        (let [loc (select-keys tok [:line :col])]
          (padvance! p)
          (apply list (parse-forms-until p :close-paren loc)))
        ;; In surface mode, parens group a single expression: (a + b) * c
        (do
          (padvance! p)
          (let [inner (parse-expr p)]
            (when-not (tok-type? (ppeek p) :close-paren)
              (errors/beme-error
                "Expected ')' to close grouped expression — use f(x) for calls, (expr) for grouping"
                (error-data p (select-keys tok [:line :col]))))
            (padvance! p)
            inner)))

      :open-bracket (maybe-call p (parse-vector p))
      :open-brace (maybe-call p (parse-map p))
      :open-set (maybe-call p (parse-set p))

      :deref
      (do (padvance! p)
          (let [inner (parse-form p)]
            (when (discard-sentinel? inner)
              (errors/beme-error "Deref target was discarded by #_ — nothing to dereference"
                                (error-data p (select-keys tok [:line :col]))))
            (list 'clojure.core/deref inner)))

      :meta
      (do (padvance! p)
          (let [m (parse-form p)
                _ (when (discard-sentinel? m)
                    (errors/beme-error "Metadata value was discarded by #_ — nothing to attach as metadata"
                                      (error-data p (select-keys tok [:line :col]))))
                target (parse-form p)
                _ (when (discard-sentinel? target)
                    (errors/beme-error "Metadata target was discarded by #_ — nothing to attach metadata to"
                                      (error-data p (select-keys tok [:line :col]))))]
            (vary-meta target merge (cond
                                        (keyword? m) {m true}
                                        (symbol? m)  {:tag m}
                                        (map? m)     m
                                        :else
                                        (errors/beme-error
                                          (str "Metadata must be a keyword, symbol, or map — got " (pr-str m))
                                          (error-data p (select-keys tok [:line :col])))))))

      :quote
      (do (padvance! p)
          (if (tok-type? (ppeek p) :open-paren)
            ;; '(...) — Clojure S-expression syntax inside quoted lists.
            ;; Activates clj-mode: bare parens create lists, symbols don't
            ;; trigger Rule 1 calls. This matches Clojure's quote semantics.
            (let [paren-loc (select-keys (ppeek p) [:line :col])
                  prev-mode @(:clj-mode p)]
              (padvance! p)
              (vreset! (:clj-mode p) true)
              (let [forms (try
                            (parse-forms-until p :close-paren paren-loc)
                            (finally
                              (vreset! (:clj-mode p) prev-mode)))]
                (list 'quote (apply list forms))))
            (let [inner (parse-form p)]
              (when (discard-sentinel? inner)
                (errors/beme-error "Quote target was discarded by #_ — nothing to quote"
                                  (error-data p (select-keys tok [:line :col]))))
              (list 'quote inner))))

      :syntax-quote-raw
      ;; ` forms are opaque — pass through to Clojure's reader
      (let [raw (:value tok)]
        (padvance! p)
        (maybe-call p (resolve/resolve-syntax-quote raw (select-keys tok [:line :col]))))

      :unquote
      (errors/beme-error "Unquote (~) outside syntax-quote — ~ only has meaning inside `"
                         (error-data p (select-keys tok [:line :col])))

      :unquote-splicing
      (errors/beme-error "Unquote-splicing (~@) outside syntax-quote — ~@ only has meaning inside `"
                         (error-data p (select-keys tok [:line :col])))

      :var-quote
      (do (padvance! p)
          (let [inner (parse-form p)]
            (when (discard-sentinel? inner)
              (errors/beme-error "Var-quote target was discarded by #_ — nothing to reference"
                                (error-data p (select-keys tok [:line :col]))))
            (list 'var inner)))

      :discard
      ;; #_ consumes two forms: the discarded one, then its replacement.
      ;; At a boundary (EOF / closing delimiter), returns discard-sentinel
      ;; so callers that accumulate forms can skip the gap.
      ;; Otherwise, returns the next real form — this is essential for
      ;; prefix operators (@, ^, #', #()) which call parse-form expecting
      ;; a value. It also makes #_ #_ chains work: each #_ in the chain
      ;; discards the form returned by the inner #_ and returns the next.
      (do (padvance! p)
          (when (peof? p)
            (errors/beme-error "Missing form after #_ — expected a form to discard"
                               (error-data p (assoc (select-keys tok [:line :col]) :incomplete true))))
          (parse-form p) ; parse and discard
          (let [nxt (ppeek p)]
            (if (or (nil? nxt)
                    (#{:close-paren :close-bracket :close-brace} (:type nxt))
                    (end-symbol? nxt))
              discard-sentinel
              (parse-form p))))

      :tagged-literal
      (let [tag (symbol (subs (:value tok) 1))]
        (padvance! p)
        (let [data (parse-form p)]
          (when (discard-sentinel? data)
            (errors/beme-error (str "Tagged literal #" tag " value was discarded by #_ — tagged literal requires a value")
                               (error-data p (select-keys tok [:line :col]))))
          (resolve/resolve-tagged-literal tag data (select-keys tok [:line :col]))))

      :namespaced-map-raw
      ;; #:ns{} forms are opaque — pass through to Clojure's reader
      (let [raw (:value tok)]
        (padvance! p)
        (maybe-call p (resolve/resolve-namespaced-map raw (select-keys tok [:line :col]))))

      :open-anon-fn
      ;; #() — parse body as beme, collect % params, emit (fn [params] body)
      (do (padvance! p)
          (let [body (parse-form p)
                _ (when (discard-sentinel? body)
                    (errors/beme-error "#() body was discarded — #() requires a non-discarded expression"
                                      (error-data p (select-keys tok [:line :col]))))
                nxt (ppeek p)]
            (cond
              (nil? nxt)
              (errors/beme-error "Unterminated #() — expected closing )"
                                (error-data p (assoc (select-keys tok [:line :col]) :incomplete true)))

              (not (tok-type? nxt :close-paren))
              (errors/beme-error "#() body must be a single expression — use fn(args...) for multiple expressions"
                                (error-data p (select-keys nxt [:line :col]))))
            (padvance! p)
            (let [params (find-percent-params body)
                  _ (when (contains? params 0)
                      (errors/beme-error "%0 is not a valid parameter — use %1 or % for the first argument"
                                         (error-data p (select-keys tok [:line :col]))))
                  param-vec (build-anon-fn-params params)
                  body' (normalize-bare-percent body)]
              (list 'fn param-vec body'))))

      :reader-cond-raw
      ;; #? forms are opaque — pass through to Clojure's reader
      (let [raw (:value tok)]
        (padvance! p)
        (maybe-call p (resolve/resolve-reader-cond raw (select-keys tok [:line :col]))))

      ;; default
      (errors/beme-error (str "Unexpected " (describe-token tok))
                         (error-data p (select-keys tok [:line :col]))))))

(defn- metadatable?
  "Can this value carry Clojure metadata?"
  [x]
  #?(:clj  (instance? clojure.lang.IObj x)
     :cljs (implements? IWithMeta x)))

(defn- attach-ws
  "Attach :ws metadata from a token to a form, if the form supports metadata."
  [form ws]
  (if (and ws (metadatable? form))
    (vary-meta form assoc :ws ws)
    form))

(defn- parse-call-chain
  "After parsing a form, check for chained call openers: f(x)(y) → ((f x) y).
   Handles arbitrary depth: f(x)(y)(z) → (((f x) y) z).
   Skipped in clj-mode (inside quoted lists) and for discard sentinels.
   Cross-line chaining is suppressed: 'f\\n(x)' does NOT chain, only 'f(x)'
   on the same line does. This prevents multi-arity defn arity bodies from
   bleeding into the next arity's params."
  [p form]
  (let [next-tok (ppeek p)
        prev-tok (when (and next-tok (> @(:pos p) 0))
                   (nth (:tokens p) (dec @(:pos p))))
        cross-line? (and prev-tok next-tok
                         (> (:line next-tok 0) (:end-line prev-tok 0)))]
    (if (and (not @(:clj-mode p))
             (not (discard-sentinel? form))
             (not cross-line?)
             (call-opener? next-tok))
      (let [args (parse-call-args p)]
        (recur p (apply list form args)))
      form)))

(defn- parse-form
  "Parse a single beme form. Attaches :ws (leading whitespace/comments)
   from the token as metadata on the resulting form."
  [p]
  (let [ws (:ws (ppeek p))
        depth (vswap! (:depth p) inc)]
    (try
      (when (> depth max-depth)
        (errors/beme-error (str "Maximum nesting depth (" max-depth ") exceeded — input is too deeply nested")
                           (error-data p (merge {:depth depth} (when-let [tok (ppeek p)]
                                                                 (select-keys tok [:line :col]))))))
      (let [form (parse-form-base p)]
        (attach-ws (parse-call-chain p form) ws))
      (finally
        (vswap! (:depth p) dec)))))

(defn- parse-expr
  "Parse a full expression: a syntactic form plus any infix operators (Pratt parser).
   This is the primary expression entry point for surface syntax.
   When min-prec is supplied, only operators with prec >= min-prec are consumed."
  ([p]         (pratt-climb p (parse-form p) 0))
  ([p min-prec] (pratt-climb p (parse-form p) min-prec)))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn read-beme-string-from-tokens
  "Parse pre-tokenized, pre-grouped tokens into Clojure forms.
   Used by the pipeline; most callers should use beme.alpha.core/beme->forms instead."
  ([tokens] (read-beme-string-from-tokens tokens nil nil))
  ([tokens opts] (read-beme-string-from-tokens tokens opts nil))
  ([tokens opts source]
   (let [p (make-parser tokens opts source)
         trailing (:trailing-ws (meta tokens))]
     (loop [forms []]
       (if (peof? p)
         (let [wrapped (wrap-body-lets forms)]
           (cond-> wrapped
             trailing (with-meta {:trailing-ws trailing})))
         (let [form (parse-expr p)]
           (if (discard-sentinel? form)
             (recur forms)
             (recur (conj forms form)))))))))
