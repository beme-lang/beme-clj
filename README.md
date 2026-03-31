# beme-clj — begin/end and M-expressions for Clojure

[![CI](https://github.com/xpojure-lang/beme-clj/actions/workflows/ci.yml/badge.svg)](https://github.com/xpojure-lang/beme-clj/actions/workflows/ci.yml)
[![Clojure](https://img.shields.io/badge/Clojure-JVM%20%7C%20Babashka%20%7C%20ClojureScript-blue?logo=clojure&logoColor=white)](https://clojure.org)
[![License](https://img.shields.io/github/license/xpojure-lang/beme-clj)](LICENSE)

M-expressions were McCarthy's original intended syntax for Lisp (1960).
S-expressions were meant to be internal representation only — but they stuck.
beme picks up where McCarthy left off: two rules that make nesting self-evident,
while preserving Clojure's semantics exactly.

**Rule 1** — head outside the parens: `f(x y)` => `(f x y)`

**Rule 2** (optional) — `begin`/`end` instead of parens: `f begin x y end` => `(f x y)`. You never need `begin`/`end` — everything can be written with Rule 1 alone. They exist for readability in multi-line blocks.

**Escape hatch** — `'(...)` and `` `(...) `` drop back to S-expression syntax inside: `'(f (g x))` is `(quote (f (g x)))`, not a call. When you need raw Clojure forms, just quote them.

`begin` and `end` are reserved words. If you need them as symbols, escape with slashes: `/begin/` and `/end/`. The printer does this automatically for roundtrip fidelity.

Everything else is Clojure.

```clojure
;; examples/stars.beme — bb beme run examples/stars.beme
require('[cheshire.core :as json])

defn begin stars [owner repo]
  let begin
    [
      url   str("https://api.github.com/repos/" owner "/" repo)
      resp  slurp(url)
      data  json/parse-string(resp true)
      count :stargazers_count(data)
    ]

    println(str(owner "/" repo ": " count " ⭐"))
  end
end

stars("xpojure-lang" "beme-clj")
```

## Installation

Add to `deps.edn`:

```clojure
io.github.xpojure-lang/beme-clj {:mvn/version "0.5.0-alpha"}
```

Or clone and use directly:

```bash
git clone https://github.com/xpojure-lang/beme-clj.git
cd beme-clj
```

All namespaces live under `beme.alpha` to signal that the API is pre-1.0 and may change. When the API stabilizes, namespaces will move to `beme`.

## Getting Started

Run a `.beme` file:

```bash
$ bb beme run hello.beme                                # Babashka
$ clojure -T:beme run :file '"hello.beme"'              # Clojure JVM
Hello, world!
```

Interactive REPL:

```bash
$ bb beme repl                                          # Babashka
user=> +(1 2)
3
user=> map(inc [1 2 3])
(2 3 4)
```

Convert between beme and Clojure (direction detected from extension):

```bash
$ bb beme convert hello.beme                            # .beme → Clojure
$ bb beme convert hello.clj                             # .clj → beme
$ clojure -T:beme convert :file '"hello.beme"'          # Clojure JVM
```

Format `.beme` files (normalize syntax via pprint):

```bash
$ bb beme format hello.beme                             # in-place
$ bb beme format src/                                   # directory, recursive
```

Requires [Babashka](https://babashka.org) or [Clojure](https://clojure.org).

## Editor Support

| Editor | Repository | Features |
|--------|-----------|----------|
| [Zed](https://zed.dev) | [zed-beme](https://github.com/xpojure-lang/zed-beme) | Syntax highlighting, brackets, indentation, symbol outline |
| [VS Code](https://code.visualstudio.com) | [vscode-beme](https://github.com/xpojure-lang/vscode-beme) | Syntax highlighting, brackets, indentation, folding |

Tree-sitter grammar: [tree-sitter-beme](https://github.com/xpojure-lang/tree-sitter-beme)

## Documentation

- [Language Reference](doc/language-reference.md) — complete syntax guide
- [API Reference](doc/api.md) — public functions
- [Development](doc/development.md) — testing, architecture
- [Design Decisions](doc/design-decisions.md) — rationale behind each choice
- [Product Requirements](doc/PRD.md) — requirements and known limitations
