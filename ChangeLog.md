# Changelog for kiwi

TODO

## Unreleased

- Search: no more limit on the number of pages ranked for a query. With the
  former limit (24 pages), a reader could miss results among pages they can
  not read, or outside of the sources of the navigation.
- Browse page: sources, tags and custom metadata keys are sorted without case
  nor accents, as on the metadata page.
- Errors: a request with a missing or invalid parameter gets the "invalid
  request" page (400), any other failure of the server a "server error" page
  (500) and a line in the logs. `/search` without query shows the search
  form.
- The server uses all the cores: pages are loaded concurrently at startup
  and on reload, and a login no longer stops the other requests while its
  password is hashed.
- Responses are compressed (gzip) for the browsers that accept it.
- Pages, browse, search, metadata and doctor pages have an ETag: a browser
  that already has the page gets a 304, without rendering, until the next
  reload.
- **Pages with an invalid header are refused.** The first block of a page
  must be its metadata: `field: value` lines, and `---` or `...`
  separators. A page starting with a heading or some text, or with a free
  line in its header, is no longer loaded. A page without metadata starts
  with an empty line.
- Doctor page: for admins, a "Load errors" section lists the files that are
  not loaded, with the reason.
- New `bind` setting in the `[kiwi]` section: the address to listen on
  (`127.0.0.1` in a new *kiwi.ini*). Without it, all the interfaces, as
  before.
- **The lines of the `_accounts` file start with a version number.** Add
  `1:` at the start of each line of your `_accounts` files when updating
  (`paul:salt:hash:admin` becomes `1:paul:salt:hash:admin`): the lines
  without it are ignored, so nobody can log in until it is done.
- New command `kiwi reload CONFIG_FILE`: asks the running server to reload
  its pages and accounts, as the reload button does. `kiwi serve` writes a
  `_pid` file next to *kiwi.ini* for it.
- Search results are listed one by line, each one with an excerpt of the
  page around the first word of the query, these words in bold.
