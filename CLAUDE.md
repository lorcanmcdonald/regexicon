# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Regexicon is a web service that generates random strings matching a given regular expression. Live at [regexicon.com](http://regexicon.com).

## Build Commands

**Development (Nix environment recommended):**
```bash
cabal build                    # Build the project
cabal new-test                 # Run tests
```

**Interactive development with watch mode:**
```bash
script/dev                     # Uses ghcid to watch and run tests
```

**Full Docker-based build:**
```bash
script/update                  # Builds Haskell Docker image, runs cabal build, builds JS
script/server                  # Starts dev server with webpack watch
script/test                    # Runs tests with coverage in Docker
```

## Testing

The test suite uses Tasty with QuickCheck for property-based testing.

```bash
cabal new-test                              # Run all tests
cabal v2-repl test-matching                 # Load tests in REPL
TASTY_HIDE_SUCCESSES=True cabal new-test    # Only show failures
TASTY_QUICKCHECK_TESTS=20 cabal new-test    # Override QuickCheck iteration count
```

Test groups: Parse, Subpatterns, Backslash patterns, Matching (defined in `src/Tests/Main.hs`).

## Architecture

**Haskell Backend (src/):**
- `Matching.hs` - Core API: `matches :: Int -> String -> IO RegexResults`
- `Matching/Server/Main.hs` - Scotty web server (port 80)
- `Test/QuickCheck/Regex/PCRE/` - Regex parsing and types:
  - `Parse.hs` - Parsec-based PCRE regex parser
  - `Types/` - AST types (Regex, Pattern, Quantifiable, etc.)
  - `Render.hs` - Regex rendering
- `Test/QuickCheck/Regex/Exemplify.hs` - String generation from parsed regex

**JavaScript Frontend (js/):**
- React-based UI with Preact
- `Components/Main.jsx` - Main component
- Webpack bundled, served from `/js/` route

**API Routes:**
- `GET /` - Landing page (optional `?q=` regex, `?n=` count)
- `POST /regex/?n=<count>` - Generate matching strings (JSON response)
- 20-second timeout on regex operations, max 20 results per request

## Key Dependencies

- QuickCheck for random string generation
- Parsec for regex parsing
- regex-tdfa for validation
- Scotty for web server
- Tasty for testing

## GHC Flags

The project uses `-Wall -Werror` in tests and executable. The library uses `-Wall` without `-Werror`.
