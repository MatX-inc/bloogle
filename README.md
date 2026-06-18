# Bloogle [![Build status](https://img.shields.io/github/actions/workflow/status/MatX-inc/bloogle/ci.yml?branch=master&label=Build)](https://github.com/MatX-inc/bloogle/actions)

Bloogle is a [Bluespec Classic](https://github.com/B-Lang-org/bsc) API search engine. It lets you search the Bluespec standard libraries by function name, by approximate type signature, or both. It is a fork of [Hoogle](https://github.com/ndmitchell/hoogle) retargeted from Haskell to Bluespec.

* **Online:** https://bloogle.bluespec.dev/
* **Source:** https://github.com/MatX-inc/bloogle
* **Issues:** https://github.com/MatX-inc/bloogle/issues

> [!WARNING]
> This version is "vibe debloated" to mass-remove functionality that doesn't make sense for our use-case.
> A more thoughtful port is in progress.

## Searching

A query can be text, a type signature, or a mix of the two:

* `mkReg` searches as text, finding `mkReg`, `mkRegU`, `mkRegA`
* `mk reg` searches for both words, finding `mkRegU` but not `mkFIFO`
* `a -> a` searches by type, finding `id :: a -> a`
* `:: a` forces a type search for "a"; a bare `a` searches as text
* `id :: a -> a` searches for the text "id" *and* the type "a -> a"

### Type search

* Matches are by exact arity: `a -> a` won't match `a -> a -> a`
* Type variables are unified and argument order is ignored
* `_` wildcards a single argument's type

See [`docs/TypeSearch.md`](docs/TypeSearch.md) for how type search works under the hood.

### Scope

Restrict or exclude results by module:

* `fold +Vector` restricts results to the `Vector` module
* `reg -Vector` excludes the `Vector` module

## Usage

Bloogle always needs a database to read or build — there is no default
database location, and it never downloads anything. Every command takes an
explicit `--database`.

### Build a database

Generate a database from a directory of Bluespec API docs in Hoogle's input
`.txt` format (one file per package, each beginning with `@package`):

    $ bloogle generate --local=path/to/docs --database=bluespec.hoo

You can also point at a directory of Haddock-style output with
`--haddock=DIR`. Generating from an online package set is not supported, so a
bare `bloogle generate` is an error — pass `--local` or `--haddock`.

### Search from the command line

    $ bloogle search --database=bluespec.hoo "Vector n a -> a"

Quote the query so the shell doesn't interpret the `->` or brackets.

### Run the web server

    $ bloogle server --database=bluespec.hoo --home=https://bloogle.bluespec.dev

* `--home` sets the URL the logo links to and the base URL baked into the
  OpenSearch descriptor (`search.xml`). It defaults to `http://localhost:8080`
  for local development; set it to your public address when deploying.
* `--port` chooses the listen port (default 8080).
* Prometheus metrics are exposed at `/metrics`.

Once the server is running, browsers can add Bloogle as a search engine from
the OpenSearch descriptor it serves, so you can search from the address bar.

## Building from source

Bloogle is a Haskell project built with `cabal` (GHC 9.4 or newer):

    $ git clone git@github.com:MatX-inc/bloogle.git
    $ cd bloogle
    $ cabal build
    $ cabal run bloogle -- <args>      # e.g. generate / search / server

The web front-end in `html/` is plain HTML, CSS, and dependency-free vanilla
JavaScript — there is no front-end build step. The test suite (code tests plus
a self-contained sample-database test) runs with:

    $ cabal run bloogle -- test

`.ghci` provides a GHCi-based dev workflow (`:opt`, `:test`, etc.) for building
and running without cabal.

## Project structure

| Directory | Contents |
|-----------|----------|
| `src`     | Haskell source |
| `cbits`   | C implementation of the text search |
| `html`    | web front-end (HTML, CSS, vanilla JS, images) |
| `misc`    | logo, keyword list, sample data |
| `docs`    | additional documentation (parts are inherited from upstream Hoogle and still describe Haskell) |

## Relationship to Hoogle

Bloogle is a fork of [Hoogle](https://github.com/ndmitchell/hoogle) by Neil
Mitchell. The core search engine, type-search algorithm, and web UI all derive
from Hoogle. The main differences:

* Indexes Bluespec libraries instead of Haskell/Stackage
* Generates only from local sources (`--local` / `--haddock`) — no
  Hackage/Stackage download
* Requires an explicit `--database` (no default location)
* Dependency-free front-end (no jQuery) and a Prometheus `/metrics` endpoint

## License

[BSD-3-Clause](LICENSE). &copy; Neil Mitchell 2004-present; Bluespec adaptations
&copy; MatX.
