# hakyll-notes

Create a static wiki from org-mode note files with a biblatex bibliography using hakyll.
It

* creates a standalone HTML bibliographies from a BibLaTeX file
* converts org files to HTML adding links to the bibliography
* creates an index of all org and BibLaTex files.

## Compiling

hakyll-notes uses [stack](https://docs.haskellstack.org/en/stable/README/).

```
$ stack setup
$ stack build
```

Both commands can take some time on the first run.
`stack setup` installs a specific version of the Haskell compiler
and `stack build` builds the project including all dependencies.

## Building the Site

Put some org-mode files, a `bibliography.bib`, and a `bibliography.org`
into a subdirectory `notes/` and run

```
$ stack exec site build
```

The site should appear in `_site/` and you can serve it locally using

```
$ stack exec site server
```

See

```
$ stack exec site
```

for more Hakyll commands.

## Configuration

hakyll-notes is just a [Hakyll](https://jaspervdj.be/hakyll/) project,
feel free to adapt it to your needs.
Some things to configure are:

* `site.hs` as the main hakyll configuration file
  * e.g., the variables `bibFile`, `cslFile`, and `paperNotesFile`.
* the CSS in `css/`
* the templates in `templates/`

After changing `site.hs` , recompile it with `stack build`.
Tip: during development with Emacs/Intero,
load `site.hs` to the intero repl and run commands with `:main command`.
Remeber to run `clean` or `rebuild` on the site after recompiling `site.hs`.

Only touch `hakyll-note.cabal` and `stack.yaml` if you know what you are doing.

## Workflow Example

This project was created to compile notes with citations to a static wiki for remote/mobile access.
One possibility to do this is the following:

### Keeping Notes

Keep your notes in a git repository together with a `bibliography.bib`.
Use [org-ref](https://github.com/jkitchin/org-ref) to manage citations in your org files.
Add `bibliography.bib` either as a `bibliography` link in each org file
or set it as a global bibliography using `org-ref-default-bibliography`.
Likewise, set `bibliography.org` as the notes file for the bibliography.

Pandoc (which Hakyll uses to convert documents) can automatically handle citations
in org-ref notation.
This feature is not really well documented in Pandoc,
only `[@ref]`-style citations for Markdown are mentioned.
hakyll-notes modifies the Pandoc output by adding a link
to the corresponding entry in the html version of the bibliography file
(see `compileNote` and `compileBib` in `site.hs`).

### Automatic Compilation

Set up hakyll-notes in a directory either on your machine or on a server,
depending on whether you keep your notes locally or in a remote git repository.

Add a hook to your notes git repo which does the following
on each commit (local) or push (remote):

1. switch to the hakyll-notes directory
2. pull the notes repo into the `notes/` directory
3. build the site (using `stack exec site build` or `rebuild`)
4. (optional) deploy the contents of `_site` somewhere.

Steps here are 2 and 4 depend on the details of your setup,
important is only that you somehow get you notes into `notes/`
and the result from `_site` to where you need it.

**NOTE:**
If you build hakyll-notes on a server, especially on shared hosting,
you might run into problems with memory usage while compiling the depencies.
`stack_server.yaml` adds the `+RTS -M... -RTS` to limit memory usage during compilation.
You can use it by adding `--stack-yaml stack_server.yaml` to your stack commands.

## TODO

* Somehow use `.dir-locals.el` to set the bibliography file etc. relative to the notes repo.
