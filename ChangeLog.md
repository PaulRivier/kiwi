# ChangeLog for Kiwi

## Unreleased

- The `root-tag` setting of the `[source NAME]` sections is removed: the
  tags of the pages are those of their header. The key is ignored in an
  existing `kiwi.ini`.
- New `summary` metadata (optional): a short description of the page,
  shown as the title (tooltip) of the links to the page, after the title
  of the link if it has one.
- **API for the agents**: `/api/search` (any of the words, the pages with
  the most of them first), `/api/pages` (`SOURCE/UID`, and the fields of
  `show`), `/api/facets` (tags and fields of the pages selected, with
  their number of pages, to explore the wiki step by step), `/api/links`
  (the links between pages), `/api/page/SOURCE/UID` (lines counted from
  the first line of the content) (with its outline, sections
  and lines), `/api/page/SOURCE/UID/meta`, `/api/folders/SOURCE` and
  `/api/guide`, in compact text, with API tokens (`kiwi access --token`,
  `--revoke-tokens`; an admin logged in on the web can also read the `GET`
  routes in the browser). New `agent-guide` setting: a file of
  instructions given to the agents after the documentation of the API. Writes: `PATCH` (exact
  passages), `PUT` (whole text, `If-Match`), creation (`POST
  /api/pages/SOURCE`) and metadata scripts (`POST /api/metadata`: on the
  pages of `pages SOURCE/*,SOURCE/UID,…` lines, move, delete, rename, set,
  add, remove or unset tags and fields; `check=yes` to try without
  writing, also on the pages that the account can only read; `message=`
  adds a message to the commit of a write; syntax in `/api/metadata-guide`), committed with the account as
  author; the admins run them from the metadata editor
  (`/metadata-editor`, linked from the metadata page).
- Custom metadata can have a `description` in `kiwi.ini`, shown on the
  metadata page and given to the agents with the facets.
- **MCP**: the server serves the MCP of the AI agents on `POST /mcp`
  (Claude Code…), with the API token; its tools call the API.
  `scripts/kiwi-mcp-bridge.py` is a stdio bridge for the agents that need
  one (Chat tab of Claude Desktop). An agent with a shell (Claude Code)
  can change a page as a local file with curl, when your agent guide
  gives the URL and the variable of the token (see *doc/AGENTS.md*).

## v0.4.4.0 - 2026-11-16

The source layout format is changing. Reach out if you need migration code.

- **Page uids**: each page has a `uid` metadata, unique in its source.
  Kiwi writes one (10 random characters) in a page that has none when it
  loads it, and gives a new one to a page whose uid is invalid or already
  the one of another page of the source, the former uid going to the new
  `previous-uids` metadata. The file is written again with a normalized
  metadata header, and committed (one commit by repository, "Kiwi: uid
  assigned to N pages"). A page whose file can not be written is not
  loaded, and listed on the Doctor page. The page address is
  `/page/SOURCE/UID`, and the links to a page are `[text](page:UID)` or
  `[text](page:UID#anchor)`: they no longer depend on the path of its file.
  The copied link of a page uses its uid. `uid` and `previous-uids` can not
  be changed from the browser editor, whatever the right.
- **Breaking**: the `page:` links with a path and the `id` metadata are no
  longer supported (a `page:` link targets a uid). Page files with Windows
  line endings (CRLF) or a byte order mark are no longer loaded (the browser
  editor saves LF line endings).
- The pages created by `kiwi init` have the uids `Home` and `CommonMark`.
- **Free layout of the sources**: pages, images and other files go
  anywhere in the folder of a source (no more `pages`, `images` and
  `files` folders). Hidden files and folders, and those whose name starts
  with `_`, are ignored.
- **Files served by their links**: a file of a source is served only when
  a page links to it with `file:`, and only to the readers of at least one
  of these pages, archived or not (401/403 otherwise, 404 when no page
  links to it). The files of the pages, the copies left by editors
  (`page.md~`, `#page.md#`…), and the temporary and backup files (see the
  README) are never served. The files are
  revalidated by the browser at each use (ETag), and the text files are
  served as UTF-8. A file link to an image opens the image viewer, by the
  extension of the file.
- **Breaking**: the `image:` links, the `/image/` addresses and the
  `images-dir` and `files-dir` metadata are no longer supported: use
  `file:` (see the file uids below). Move the content of `pages`, `images`
  and `files` to the top of the folder of each source before starting this
  version.
- **File uids**: each file of a source that is not a page gets a uid,
  unique in its source, written in its name: Kiwi renames `cake.jpg` to
  `cake__K3M9QW2TZ1.jpg` when it loads it, and commits the rename. A file
  is linked by its uid (`![a cake](file:K3M9QW2TZ1)`), and served at
  `/file/SOURCE/UID` with its name without uid: it can be renamed or moved
  anywhere in its source without breaking its links. A copy of a file gets
  a new uid. Temporary and backup files (`x.tmp`, `x.part`, `x.bak`,
  `~$x.docx`, syncthing conflict copies…) get no uid and are never served.
  The Doctor page lists the links to missing files and, for admins, the
  files that no page links, with a button copying a link to each one.
- **Breaking**: the `file:` links with a path are broken (a `file:` link
  targets a uid); the `/file/SOURCE/path` addresses no longer work.
- **Search by uid**: the search `uid:UID` shows the page and the file with
  this uid, and the pages that link to it.
- The Doctor and Metadata pages have the reload button (admins).
- Git: the commits of Kiwi include the former path of a renamed file.
- Git: before committing a page saved from the browser, Kiwi commits the
  changes of its file made outside of Kiwi (or the file not tracked yet),
  with "Kiwi" as author and the message "Pre-update commit of SOURCE:UID
  (kiwi NAME)", so that they are not attributed to the user of the browser.
  The time limit of git is now 60 seconds.

## v0.4.2.0 - 2026-11-13

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
  git repository (and not ignored there): the file alone, with "LOGIN via
  Kiwi" as author. New `git-commit` setting in the `[kiwi]` section
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
