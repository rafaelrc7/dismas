# Dismas

Scrape and read the Holy Bible right from your terminal!

This software was just made public, so some defaults were personal and will be
changed in the future. The default used version is the RSVCE, scraped from
[biblegateway](https://www.biblegateway.com).

This README is also WIP.

## Install

### Cabal

To download and build from source you will need Haskell and Cabal. Take a look
at: https://www.haskell.org/downloads/

I recommend using GHCup.

Clone the repo and run:

```sh
cabal build
cabal install
```

### Nix

There are many options. The repository flake provides the package and an
overlay.

## Usage

```
dismas-dl

Usage: dismas-dl [-d|--base-dir BASE DIR] [-s|--short-name NAME]
                 [-s|--long-name NAME] [-y|--no-confirm]

  Download bibles from biblegateway.com

Available options:
  -d,--base-dir BASE DIR   Use BASE DIR as base directory for bibles download.
                           The short version name gets appended to it.
                           (default: "~/.local/share/bibles")
  -s,--short-name NAME     Use NAME as the short version name, found in the
                           search window url (default: "RSVCE")
  -s,--long-name NAME      Use NAME as the long version name, found in the book
                           list url
                           (default: "Revised-Standard-Version-Catholic-Edition-RSVCE-Bible")
  -y,--no-confirm          Do not prompt for download confirmation
  -h,--help                Show this help text
  --version                Show version information
```

```
dismas

Usage: dismas [-d|--base-dir DIR] [-n|--version-name NAME] [-w|--width WIDTH]
              REFERENCE...

  Read offline holy bible

Available options:
  -d,--base-dir DIR        Use DIR as base directory for bibles. The version
                           name gets appended to it.
                           (default: "~/.local/share/bibles")
  -n,--version-name NAME   Use NAME as the version name (default: "RSVCE")
  -w,--width WIDTH         Use WIDTH as the rendered text width (default: 80)
  -h,--help                Show this help text
  --version                Show version information

Reference:
    <Book>
        Individual book
    <Book>:<Chapter>
        Individual chapter of a book
    <Book>:<Chapter>:<Verse>[,<Verse>]...
        Individual verse(s) of a specific chapter of a book
    <Book>:<Chapter>-<Chapter>
        Range of chapters in a book
    <Book>:<Chapter>:<Verse>-<Verse>
        Range of verses in a book chapter
    <Book>:<Chapter>:<Verse>-<Chapter>:<Verse>
        Range of chapters and verses in a book
    /<Search>
        All verses that match a pattern
    <Book>/<Search>
        All verses in a book that match a pattern
    <Book>:<Chapter>/<Search>
        All verses in a chapter of a book that match a pattern
```
