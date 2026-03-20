# beme-clj — begin/end delimiters and M-expressions for Clojure

[![CI](https://github.com/beme-lang/beme-clj/actions/workflows/ci.yml/badge.svg)](https://github.com/beme-lang/beme-clj/actions/workflows/ci.yml)
[![Clojure](https://img.shields.io/badge/Clojure-JVM%20%7C%20Babashka%20%7C%20ClojureScript-blue?logo=clojure&logoColor=white)](https://clojure.org)
[![License](https://img.shields.io/github/license/beme-lang/beme-clj)](LICENSE)

```
;; beme                                     ;; Clojure
defn(greet [name]                           (defn greet [name]
  println(str("Hello, " name "!")))           (println (str "Hello, " name "!")))

->>(accounts                                (->> accounts
  filter(:active)                              (filter :active)
  map(:balance)                                (map :balance)
  reduce(+))                                   (reduce +))

let([x 1, y +(x 1)]                        (let [x 1, y (+ x 1)]
  *(x y))                                     (* x y))
```

Two rules. Everything else is Clojure.

## The Rules

**Rule 1** — the head of a call goes outside the parens:

```
f(x y)                        => (f x y)
```

**Rule 2** — `begin`/`end` as textual delimiters:

```
f begin x y end               => (f x y)
```

That's it. No new semantics — just syntax.

## In Action

```
;; Definitions
def(x 42)
defn(greet [name] str("Hello, " name "!"))

;; Multi-arity
defn(greet
  [name](greet(name "!"))
  [name punct](println(str("Hello " name punct))))

;; Bindings & destructuring
let([{:keys [id name]} person]
  println(id name))

;; Threading
->>(accounts
  filter(:active)
  map(fn([a] update(a :balance *(:balance(a) 1.05))))
  remove(fn([a] neg?(:balance(a))))
  reduce(+ 0))

;; Control flow
cond(
  >(x 0)   "positive"
  ==(x 0)  "zero"
  :else    "negative")

loop([i 0, acc []]
  if(>=(i 10)
    acc
    recur(inc(i) conj(acc i))))

;; Error handling
try(
  dangerous-operation()
  catch(Exception e
    log/error("Failed:" e)
    default-value)
  finally(
    cleanup()))

;; Namespace
ns(my.app
  :require(
    [clojure.string :as str]
    [my.db :refer [query connect]])
  :import(
    [java.util Date UUID]))

;; Java interop
.toUpperCase("hello")
Math/abs(-1)
java.util.Date.()

;; Protocols
defprotocol(Drawable
  draw([this canvas])
  bounds([this]))

defrecord(Circle [center radius]
  Drawable
  draw([this canvas] render-circle(canvas center radius))
  bounds([this]
    {:x -(:x(center) radius)
     :y -(:y(center) radius)}))

;; Concurrency
def(state atom({:count 0}))
swap!(state update(:count inc))
@state

;; Begin/end for multi-line blocks
defn begin greet [name]
  println(str("Hello " name))
end
```

All data literals, reader syntax (`@`, `^`, `#'`, `#_`, `#{}`, `#""`, `#?()`, `#:ns{}`),
destructuring, and commas-as-whitespace work exactly as in Clojure.

## No Quoting

In Clojure, `'()` exists because `()` is both a call and a list.
In beme, `()` is always a call. `[]` is always data.
Need a list? `list(1 2 3)`.

## Full Example

```
ns(my.accounts
  :require([clojure.string :as str]))

defn(transform-accounts [accounts]
  let([
    active filter(:active accounts)
    balanced ->>(active
      map(fn([a] update(a :balance *(:balance(a) 1.05))))
      remove(fn([a] neg?(:balance(a)))))
  ]
    reduce(fn([acc {:keys [id balance]}]
      assoc(acc id {:balance balance
                    :status :processed
                    :updated-at inst-ms(java.util.Date.())}))
    {} balanced)))

defn(summarize [accounts]
  let([
    processed transform-accounts(accounts)
    total ->>(processed vals() map(:balance) reduce(+))
  ]
    println(str("Total: " total))
    processed))
```

<details>
<summary>Equivalent Clojure</summary>

```clojure
(ns my.accounts
  (:require
    [clojure.string :as str]))

(defn transform-accounts [accounts]
  (let [active (filter :active accounts)
        balanced (->> active
                   (map (fn [a] (update a :balance (* (:balance a) 1.05))))
                   (remove (fn [a] (neg? (:balance a)))))]
    (reduce (fn [acc {:keys [id balance]}]
              (assoc acc id {:balance balance
                             :status :processed
                             :updated-at (inst-ms (java.util.Date.))}))
            {}
            balanced)))

(defn summarize [accounts]
  (let [processed (transform-accounts accounts)
        total (->> processed
                (vals)
                (map :balance)
                (reduce +))]
    (println (str "Total: " total))
    processed))
```

</details>

## Getting Started

```bash
bb beme                # Start the REPL
bb beme-run file.beme  # Run a .beme file
```

Requires [Babashka](https://babashka.org) or [Clojure](https://clojure.org).

## REPL

```
beme=> +(1 2)
3
beme=> map(inc [1 2 3])
(2 3 4)
```

Multi-line input continues until balanced.

## Testing

```bash
bb test-beme           # Babashka example + fixture tests
clojure -X:test        # JVM unit tests
bb test-cljs           # ClojureScript tests (needs Node.js)
bb test-all            # All three suites
```

## Architecture

```
.beme file -> tokenizer -> grouper -> parser -> Clojure forms -> eval
```

The reader and printer are pure functions (`.cljc`), portable across JVM, Babashka, and ClojureScript. No runtime dependency. beme is a reader, not a language.

## Why

M-expressions were McCarthy's original intended syntax for Lisp (1960).
S-expressions were meant to be internal representation only — but they stuck.
beme picks up where McCarthy left off: two rules that make nesting self-evident,
while preserving Clojure's semantics exactly.
