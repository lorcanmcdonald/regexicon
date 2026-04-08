# PCRE2 Spec Coverage

Analysis of parser and generator coverage against the [PCRE2 spec](https://github.com/PCRE2Project/pcre2/blob/main/doc/pcre2pattern.3), grouped by the functions/types expected to handle each feature.

## `backslashSequence` / `BackslashSequence`

The catch-all case `Nonalphanumeric <$> satisfy (not . isAlphaNum)` handles non-alphanumeric escapes but **all of the following are alphanumeric and fall through, causing a parse failure**:

| Syntax | Description | Status |
|--------|-------------|--------|
| `\a` | BEL (hex 07) | ✅ Implemented |
| `\e` | ESC (hex 1B) | ✅ Implemented |
| `\f` | Form feed | ✅ Implemented |
| `\n` | Linefeed | ✅ Implemented |
| `\r` | Carriage return | ✅ Implemented |
| `\t` | Tab | ✅ Implemented |
| `\cX` | Control character | ❌ Commented out (`Backslashes.hs:27`) |
| `\d`, `\D` | Digit / non-digit | ✅ Implemented |
| `\h`, `\H` | Horizontal whitespace / negation | ✅ Implemented |
| `\s`, `\S` | Whitespace / negation | ✅ Implemented |
| `\v`, `\V` | Vertical whitespace / negation | ✅ Implemented |
| `\w`, `\W` | Word character / negation | ✅ Implemented |
| `\b`, `\B` | Word boundary / non-word boundary | ❌ Not implemented |
| `\A` | Start of subject | ❌ Not implemented |
| `\Z` | End of subject (before optional newline) | ❌ Not implemented |
| `\z` | Absolute end of subject | ❌ Not implemented |
| `\G` | First matching position | ❌ Not implemented |
| `\K` | Reset start of match | ❌ Not implemented |
| `\N` | Not a newline | ❌ Not implemented |
| `\R` | Any newline sequence | ❌ Not implemented |
| `\X` | Unicode extended grapheme cluster | ❌ Not implemented |
| `\p{...}`, `\P{...}` | Unicode properties | ❌ Not implemented |
| `\g{n}`, `\gn` | Unambiguous backreference syntax | ❌ Not implemented |
| `\N{U+hhhh}` | Unicode char by code point | ❌ Not implemented |

---

## `metacharacter` / `Metacharacter`

| Syntax | Description | Status |
|--------|-------------|--------|
| `*` | Zero or more (greedy) | ✅ `ZeroOrMore` |
| `+` | One or more (greedy) | ✅ `OneOrMore` |
| `{n,m}` | Between n and m (greedy) | ✅ `MinMax` |
| `?` | Zero or one | ✅ `ZeroOrOne` |
| `{n}` | Exactly n | ✅ `MinMax` (parsed as `{n,n}`) |
| `{n,}` | n or more (unbounded) | ❌ Not implemented |
| `*?`, `+?`, `??`, `{n,m}?` | Lazy/non-greedy variants | ❌ Not implemented |
| `*+`, `++`, `?+`, `{n,m}+` | Possessive variants | ❌ Not implemented |

---

## `quantifiable` / `Quantifiable`

| Syntax | Description | Status |
|--------|-------------|--------|
| `.` | Any character | ✅ `AnyCharacter` |
| `c` | Literal character | ✅ `Character` |
| `\...` | Backslash sequence | ✅ `Backslash` |
| `(...)` | Capturing group | ✅ `Subpattern` |
| `[...]` | Character class | ✅ `CharacterClass` |
| `[^...]` | Negated character class | ✅ `NegatedCharacterClass` |
| `\1`, `\2`... | Backreference | ✅ `BackReference` (resolved from `AmbiguousNumberSequence`) |
| `(?:...)` | Non-capturing group | ❌ Not implemented |
| `(?=...)`, `(?!...)` | Lookahead assertions | ❌ Not implemented |
| `(?<=...)`, `(?<!...)` | Lookbehind assertions | ❌ Not implemented |
| `(?>...)` | Atomic group | ❌ Not implemented |
| `(?<name>...)` etc. | Named capturing groups | ❌ Not implemented |
| `(?|...)` | Branch reset group | ❌ Not implemented |
| `(?#...)` | Comment | ❌ Not implemented |
| `(?(cond)yes\|no)` | Conditional | ❌ Not implemented |
| `(?R)`, `(?n)`, `(?&name)` | Recursion / subroutine calls | ❌ Not implemented |

---

## `characterClassCharacters` / `ClassBackslashSequence`

Only `\-` (`CCHyphen`) and `\d` (`CCDigit`) are handled inside `[...]`. All other backslash sequences fail to parse within a character class.

| Syntax | Description | Status |
|--------|-------------|--------|
| `\-` | Escaped hyphen | ✅ `CCHyphen` |
| `\d` | Digit class | ✅ `CCDigit` |
| `a-z` | Character range | ✅ `ClassRange` (parsing works; generation disabled for performance) |
| `\Q...\E` | Quoted literals | ✅ `QuotedClassLiterals` (parsing works; generation disabled) |
| `\w`, `\W`, `\s`, `\S` | Character class shorthands | ❌ Not implemented |
| `\h`, `\H`, `\v`, `\V` | Whitespace shorthands | ❌ Not implemented |
| `\n`, `\r`, `\t`, `\a`, `\e`, `\f` | Non-printing chars | ❌ Not implemented |
| `\b` | Backspace (hex 08; inside `[...]` only) | ❌ Not implemented |
| `\p{...}`, `\P{...}` | Unicode properties | ❌ Not implemented |
| `[:alpha:]`, `[:digit:]` etc. | POSIX named classes | ❌ Not implemented |

---

## `regex` / top-level `Regex`

| Syntax | Description | Status |
|--------|-------------|--------|
| `^pattern` | Start anchor | ✅ Implemented |
| `pattern$` | End anchor | ✅ Implemented |
| `a\|b` | Alternation | ✅ Implemented |
| `(?i)`, `(?m)`, `(?s)`, `(?x)` | Inline option flags | ❌ Not implemented |
| `(*UTF)`, `(*UCP)`, `(*CR)` etc. | Start-of-pattern mode modifiers | ❌ Not implemented |

---

## Suggested priority order

1. **`\b`/`\B`/`\A`/`\Z`/`\z`** — common anchors, straightforward `BackslashSequence` additions
2. **`(?:...)`** — very common, structurally mirrors existing `Subpattern`
3. **Lazy quantifiers** — common in real-world regexes
4. **Extended `ClassBackslashSequence`** — fills out character class coverage
5. **Lookaheads/lookbehinds** — moderate complexity
6. **Named groups, conditionals, recursion** — significant complexity, lower urgency

---

## Testing

Round-trip tests live in `src/Tests/RoundTripTests.hs` and are structured for red/green tracking:

- **`Round-trip > Implemented`** — QuickCheck properties and specific cases for all working features; these should pass.
- **`Round-trip > Not yet implemented`** — One `assertBool (isRight (parseRegex ...))` test per unimplemented feature; these fail until the feature is added. When implementing a feature, move its test into the implemented group and tighten the assertion to check the full expected AST.

The `Exemplify` typeclass provides the complementary direction — given a parsed `Regex`, generate strings that should match it — verified against `regex-tdfa` in the `Matching` test group.
