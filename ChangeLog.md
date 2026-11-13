# ChangeLog for Kiwi

## v0.4.1.0 - 2026-11-13

- **Edit pages in the browser**, with a markdown editor (EasyMDE, embedded:
  nothing loaded from elsewhere), a toolbar and a preview rendered by Kiwi.
  A pen in the navigation bar opens the editor, and a small pen in the
  margin of a block opens it at the line of this block. The file is written
  atomically, with a normalized metadata header (one line by key, fixed
  order, sorted tags), and the pages are reloaded. A save is refused when
  the file has changed since the editor was opened, or when the page would
  not load.
- **Access rights per group**: `family` or `family:r` (read), `family:w`
  (read and edit the page, except its `access`, `id`, `archive`,
  `images-dir` and `files-dir` metadata), `family:a` (edit everything).
  `public` can only read. Existing access lists keep their meaning.
- **Pages saved from the browser are committed** when their file is in a
  git repository (and not ignored there): the file alone, with the login of
  the user as author. New `git-commit` setting in the `[kiwi]` section
  (`yes` by default, `no` to disable). A failure of git is logged, the page
  is saved anyway.
- The local editor button has its own icon; the pen is the browser editor.

## v0.4.1.0 - 2026-11-12

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
  page around the first word of the query, these words in bold. A search
  shows the 12 best pages at most.
- The name of a custom metadata is a link to the pages having it, whatever
  its value (`has=author` in the browse URL): on the browse page, at the top
  of a page, and on the metadata page (pages of the source).
- Metadata page: the tags, then the language and access fields of kiwi (in
  their own color), then the custom metadata in alphabetical order.
- Browse URLs read the keys of custom metadata as the pages do: a boolean
  can be `yes`, `true`, `1`, `no`, `false` or `0`.
