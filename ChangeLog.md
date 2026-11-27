# ChangeLog for Kiwi

## Unreleased

- The `archive` key of a page is no longer reserved to the admins: the
  users with the write right on the page see and change it in the editor
  and the API, as the metadata scripts already allowed.
- A page whose file changed outside of Kiwi (a local editor, a
  synchronization) is reloaded before it is read or written by the API or
  the browser editor, without waiting for a reload of the kiwi.
- **The API and the MCP tools read and write the header and the body of a
  page apart**, each one with its own hash (a change of the header does
  not change the version of the body, nor the opposite), the lines
  numbered from the first line of the body:
  - routes: `GET`/`PUT /api/page-meta/SOURCE/UID`, `GET`/`PUT`/`PATCH
    /api/page-body/SOURCE/UID`; `POST /api/pages/SOURCE` takes a JSON body
    `{"meta": "…", "body": "…"}`; the routes `/api/page/SOURCE/UID` are
    removed;
  - MCP tools: `read_page_meta`, `write_page_meta`, `read_page_body`,
    `edit_page_body_by_replace`, `write_page_body`, and `create_page` with
    `meta` and `body`; `read_page`, `edit_page` and `write_page` are
    removed (update the permissions given to them, and the agent guides
    that name them);
  - a write of the body keeps the header of the file as written, and the
    opposite.

## v0.4.6.0 - 2026-11-25

- The browser editor has a field for the message of the change, added to
  the one of the commit (not shown with `git-commit: no`).
- Logging out forgets the drafts of the browser editor kept by the
  browser.
- **Creation of pages in the browser**: a new page button in the
  navigation bar, for the users who can write in a source, opens the
  editor on a new page of the source of the page shown (`/new/SOURCE`), or
  the choice of the source (`/new`); the file is named after the title,
  or, for the admins, given by its path in the source.
- **Deletion of pages in the browser**, for the users with the admin
  right on the page, from its editor: a confirmation shows the pages
  linking to it and the files that only it links, which can be deleted
  with it. Only in a git repository (with `git-commit`): the changes not
  committed are first committed by Kiwi, then the deletion by the user.
- The page view shows the summary of the page below its title, in the
  frame of the metadata, and its keywords after the backlinks, each one a
  search of the pages that have it.
- The web search results show the summary of each page between its title
  and the excerpt.
- The search finds the words of the summary of the pages (between the
  tags and the content), `/api/search` names it among the fields found,
  and the exact phrases are also looked for in it.
- The keywords count the summary of the page.
- At most 10 keywords by page instead of 20, the words repeated in the
  page weighing more against the rare ones, in lower case, without the
  words of 1 or 2 letters.
- The keywords are updated at each load for the pages that changed only,
  all of them being computed again once a tenth of the pages changed: a
  reload no longer computes them for the whole wiki, and they are always
  ready (no more background computation).
- **Related pages** below each page, in place of the backlinks frame: one
  list "Related pages" with the backlinks first (marked by an icon), then at
  most 5 pages close to it, in the sources of the context: common keywords
  (by stem), tags (with their parents) and values of the custom fields
  with a `relating: WEIGHT` in their `[metadata NAME]` section (0 by
  default: left out). `show=related` gives them in `/api/pages` and the MCP tool
  `list_pages`, as `SOURCE/UID`.
- The pages a user can read are no longer a copy of the index made at each
  request, but a view of it: a request of a user who can not read every
  page is much faster (about 0.02 ms instead of 0.65 ms for 240 pages).

## v0.4.5.0 - 2026-11-22

This release is mostly about AI agents tooling to interact with Kiwi. Since this space evolves fast
and since I am not an AI agent myself, an AI coding agent was involved in the design. It seems to work well for most things.

- The `root-tag` setting of the `[source NAME]` sections is removed: the
  tags of the pages are those of their header. The key is ignored in an
  existing `kiwi.ini`.
- New `summary` metadata (optional): a short description of the page,
  shown as the title (tooltip) of the links to the page, after the title
  of the link if it has one.
- **API for the agents**: `/api/search` (any of the words, the pages with
  the most of them first, the filters of the lists, the words found in no
  page), `/api/pages` (`SOURCE/UID`, and the fields of
  `show`, links, backlinks and the size of the content among them), `/api/facets` (tags, fields
  and, for the admins, folders of the pages selected, with their number
  of pages, to explore the wiki step by step), `/api/page/SOURCE/UID`
  (lines counted from the first line of the content; its outline,
  sections and lines; the outline instead of a page of more than 40000
  characters, unless `part=whole`; the search results and the lists give
  the size of the pages, `content_size` and `content_lines`, and with
  `show=keywords`, by default in the MCP tools, the keywords of the pages:
  terms computed from the text of the pages, TF-IDF, stop words of
  French, English and Spanish left out), `/api/page/SOURCE/UID/meta` and `/api/guide` (topics
  `wiki`, `metadata-scripts`), in compact text, with API tokens (`kiwi access --token`,
  `--revoke-tokens`; an admin logged in on the web can also read the `GET`
  routes in the browser). New `agent-guide` setting: a file of
  instructions given to the agents after the documentation of the API. Writes: `PATCH` (exact
  passages), `PUT` (whole text, `If-Match`), creation (`POST
  /api/pages/SOURCE`) and metadata scripts (`POST /api/metadata`: on the
  pages of `pages SOURCE/*,SOURCE/UID,…` lines, move, delete, rename, set,
  add, remove or unset tags and fields; `check=yes` to try without
  writing, also on the pages that the account can only read; `message=`
  adds a message to the commit of a write; syntax in
  `/api/guide?topic=metadata-scripts`), committed with the account as
  author; the admins run them from the metadata editor
  (`/metadata-editor`, linked from the metadata page).
- Custom metadata can have a `description` in `kiwi.ini`, shown on the
  metadata page and given to the agents with the facets.
- **MCP**: the server serves the MCP of the AI agents on `POST /mcp`
  (Claude Code…), with the API token; its tools call the API, each one with
  a title and the hints of what it does (`readOnlyHint`,
  `destructiveHint`); `check_metadata` checks a metadata script, which
  `edit_metadata` applies; the tool `guide` gives the documentation by
  topic. The instructions given at the connection are three lines, in the
  context of every session of the agent (what kiwi is, and to call the tool
  `guide` first when you talk about kiwi): the documentation and your agent
  guide cost nothing until the wiki is used. `list_pages` gives the title,
  summary, size and keywords of the pages without `show`, the uids alone
  with an empty `show`. `scripts/kiwi-mcp-bridge.py` is a stdio bridge for the agents that
  need one (Chat tab of Claude Desktop). The agents change the pages with
  `edit_page` and `write_page`; working on a page as a local file with curl
  is left to your agent guide (see *doc/AGENTS.md*).

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
