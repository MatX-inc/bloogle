# Bloogle APIs

Bloogle can be used programmatically in two ways: as a Haskell library, or
over HTTP via the running server's JSON endpoint.

## Haskell library

Bloogle is a Haskell library exposing the `Bloogle` module; the command-line
tool and the web server are both built on top of it. The key entry points are
`withDatabase` / `searchDatabase` for querying a database, and `bloogle` for
running a command line operation. See `src/Bloogle.hs` for the full surface.

## JSON API

A running server (`bloogle server`) returns JSON when given `?mode=json`. The
search term is the `bloogle` query parameter (the same one the web UI uses):

    $ curl -sS "http://localhost:8080/?mode=json&bloogle=Vector%20n%20a%20-%3E%20a"

Each result is an object with the shape:

    {
      "url": "<link to the item's documentation>",
      "module":  { "url": "<module doc url>",  "name": "<module name>" },
      "package": { "url": "<package doc url>", "name": "<package name>" },
      "item": "<HTML rendering of the item, e.g. its signature>",
      "type": "",
      "docs": "<HTML rendering of the item's documentation>"
    }

The exact `url` values come from the indexed database (the `@url` directives in
the generated `.txt` data), so they point wherever your Bluespec docs live.

### Parameters

* `mode=json` — required to get JSON instead of HTML.
* `bloogle=<query>` — the search query (URL-encoded). `q=` also works.
* `start=<n>` — 1-based index of the first result to return (default 1).
* `count=<n>` — maximum number of results (default 100, capped at 500).
* `format=text` — strip the HTML tags from the `item` and `docs` fields.

For example, to get the first two results as plain text:

    $ curl -sS "http://localhost:8080/?mode=json&format=text&bloogle=mkReg&count=2"

The JSON shape may change in future versions.
