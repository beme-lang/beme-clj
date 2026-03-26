# Development

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

Pure-function reader and printer (`.cljc`), portable across JVM, Babashka, and ClojureScript. No runtime dependency. beme is a reader, not a language.

See also [Design Decisions](design-decisions.md) for rationale behind each choice.
