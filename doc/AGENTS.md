# Kiwi and the AI agents

Kiwi gives the AI agents (Claude…) and the scripts an HTTP API and an
MCP server, with the rights of an account. The rest of Kiwi is
documented in the [README](../README.md).

## API

Kiwi has an HTTP API under `/api/`, made for the AI agents (Claude…) and
the scripts: its answers are compact texts. A request needs an API token
of an account, and gets the rights of this account:

    kiwi access ~/Kiwi/kiwi.ini --token LOGIN           # displayed once
    kiwi access ~/Kiwi/kiwi.ini --revoke-tokens LOGIN
    curl -H "Authorization: Bearer TOKEN" http://localhost:3456/api/pages

The tokens are kept hashed in the *_tokens* file, next to *kiwi.ini*. A
token stops working when its account is removed or gets a new password.

An admin logged in on the web can also open the `GET` routes in the
browser, without token (for instance `/api/facets`, to see what the
agents get); the writes always need a token.

The lists of pages (`/api/pages`, `/api/facets`, `/api/search`) choose
their pages with the filters of the browse page (`source`, `tag`, `lang`,
`m.FIELD=VALUE`, `has=FIELD`, `archive=yes`; several `tag` are all
required), `since=YYYY-MM-DD` and `page=SOURCE/UID` (repeated: these pages).

* `GET /api/pages` : the pages, the most recently modified first, at most
  `limit` (100 by default): one `SOURCE/UID` by line, and under it the
  fields of `show`
  (`show=title,tags,summary,keywords,lang,modified,content_size,links,backlinks,related`
  and custom fields by their name), written as in a header. `summary`:
  the short description written in the header of the page, if any.
  `content_size`: `chars=N lines=M`, the characters and lines of its
  content, without the header. `keywords`: the terms that tell
  the page from the others (TF-IDF on the whole wiki: occurrences in
  the content, the title, the summary and the headings, rarity in the
  other pages), as written in the page, the best first, fewer for a short
  page, computed again when the page changes, never written in the files (the tool
  `list_pages` gives the title, the summary, the size and the keywords
  when no `show` is given, the uids alone with an empty `show`). `links`: the
  uids of the pages it links, with `(missing)` after a page that does
  not exist or can not be read (a broken link); `backlinks`: the uids of
  the pages that link it; `related`: the `SOURCE/UID` of the pages closest
  to it, the closest first, at most 5, in the sources of the context (by
  their common keywords, tags with their parents, and values of the custom
  fields given a `relating` weight in `kiwi.ini`; backlinks not left out),
  the same as below a page on the web, after its backlinks.
* `GET /api/search?q=QUERY` : full-text search for the agents. The pages
  that contain any of the words, those with the most of them first, each
  one with the words it contains and where (title, tags, summary,
  content), its last change (`modified`), tags, size (`content_size`), its
  summary and up to three passages (lines with their section, or
  headings); the first line names the words that no page contains.
  `"exact phrase"` and `-excluded` words are allowed, the filters of the
  lists too, `limit` (10 by default), and `show=keywords` (the keywords
  of each page, given by the tool `search`). The search page of the web
  interface is not changed.
* `GET /api/facets` : the tags, langs, folders (admins only) and custom
  fields (with their description) of the pages selected by the filters of
  `/api/pages` (all the pages without them), by source, each value with
  its number of pages (`show=tags,lang,folder,FIELD`: only these). The agents explore the wiki
  with it as a person does on the browse page: a tag, then another one…,
  and list the pages once there are few of them.
* A page has a header (its metadata, `key: value` lines) and a body (its
  content, in CommonMark, after the blank line that ends the header): the
  API reads and writes them apart, each one with its own hash (16
  hexadecimal digits of its SHA-256, as `ETag`), so that a change of the
  header does not change the version of the body, nor the opposite.
* `GET /api/page-body/SOURCE/UID` : the body of the page; a body of more
  than 40000 characters gives its outline instead, with a line that gives
  its size and says how to read it. `part=whole` gives the whole body,
  whatever its size, `part=outline` its outline (tags,
  summary, size, backlinks, sections with their lines), `part=section&title=T`
  one section, `part=lines&from=N&to=M` some lines, and `numbered=yes`
  numbers the lines. Everywhere in the API, the lines are those of the
  body, from 1.
* `GET /api/page-meta/SOURCE/UID` : the header of the page, as written in
  its file, without its uid (and without `access` for an account that has
  the write right but not the admin right). The defaults of *kiwi.ini* and
  of the source, which a page gets for a missing field when it loads, are
  not in it (`/api/pages?show=…` gives the values in use).
* A page whose file changed outside of Kiwi (an editor, a
  synchronization) is reloaded before it is read or written.
* The admins also get the path of the file of a page in its outline, and
  of a page they create.
* `GET /api/guide` : the documentation of the API for the agents,
  followed by your guide: the file given by `agent-guide: FILE` in
  *kiwi.ini* (relative to the folder of *kiwi.ini*, or absolute; read
  again at each request; the server does not start if it can not be
  read). `topic=metadata-scripts` gives the syntax of the metadata
  scripts. The MCP agents read it with the tool `guide`, once in a
  session, when you talk about kiwi (see below). Kiwi has
  no syntax of its own for the requests left to the agents in the pages:
  agree on a convention in your guide, for instance a field
  `status: claude:todo` in the header of the pages to handle (declared as
  a custom metadata), and quotes starting with `AI:` for the requests;
  your guide can also tell the agents to read pages of the wiki.

The writes need the write right on the page, as in the browser editor,
in a source where Kiwi writes (not read-only, see the README), and are
committed in the git repository of the source (author `LOGIN via Kiwi`;
`message=…` adds a message to the one of the commit):

* `PATCH /api/page-body/SOURCE/UID` : replaces exact passages of the body
  of the page, with a JSON body `{"edits": [{"old": "…", "new": "…"}]}`.
  Each old text must be found exactly once; all the edits apply, or none
  (409). The answer gives the lines of the new passages.
* `PUT /api/page-body/SOURCE/UID` : replaces the whole body, with the hash
  of the body as read in `If-Match` (412 when the body changed since, 428
  without it). The header is kept as written.
* `PUT /api/page-meta/SOURCE/UID` : replaces the header (its lines, no
  blank line), with the hash of the header as read in `If-Match`. The keys
  that the account can not change keep their value, and are listed in the
  answer; the body is kept as written.
* `POST /api/pages/SOURCE` : creates a page from a JSON body `{"meta":
  "title: …\ntags: …", "body": "…"}`. Kiwi gives it a uid (and answers
  with the hashes of its header and body, and its size), and names its
  file after its title (` (2)` is added when the name is taken); the
  admins can give its path (`path=folder/name`, without extension). The
  account must have the write right on the new page, by the access of the
  source (an admin can set it in the header).
* `POST /api/metadata` : changes the headers of many pages with a
  script, all of its commands or none, in one commit; `check=yes` writes
  nothing and lists the pages that each command would change, on the pages
  that the account can read (those it can not change are marked
  `(read only)`: an admin can then apply the script). A `pages`
  line chooses the pages of the commands after it (`SOURCE/*`,
  `SOURCE/UID`, or `*`, separated by commas), among those that the account
  can change:

      # on all the pages of a source
      pages cuisine/*
      meta tags move "cuisine > matériel" "matériel > cuisine"
      meta tags delete obsolete
      meta ingredients move bananas banana
      meta author rename-to writer
      # on a few pages
      pages cuisine/0123456789,plantes/ABCDEFGHJK
      meta summary set "First aid: wounds, burns"
      meta tags add health
      meta tags remove todo
      meta lang unset

  The syntax is in `GET /api/guide?topic=metadata-scripts`, and below the form of the
  metadata editor (`/metadata-editor`, link at the top of the *Metadata*
  page), where the admins run the same scripts: for instance a script
  written by an agent, checked then applied.

## MCP server

Kiwi serves the MCP (Model Context Protocol) of the agents on `POST
/mcp`, with the same API token. Its tools are `search`, `facets`,
`list_pages`, `read_page_meta`, `write_page_meta`, `read_page_body`,
`edit_page_body_by_replace`, `write_page_body`, `create_page`,
`edit_metadata`, `check_metadata` and `guide`: each one
calls the API, with the rights of the account. Each tool has a title and
the hints of what it does: `readOnlyHint` for the tools that only read
(Claude Code runs them in parallel), `destructiveHint` for those that
change what exists. `check_metadata` only checks a metadata script, so
that you can allow it without allowing `edit_metadata`. When it
connects, the agent gets only three lines of instructions, in the
context of every session (what kiwi is, and to call the tool `guide`
first when you talk about kiwi), so that a seldom used wiki costs
almost nothing; it reads the guide of `/api/guide` with that tool, and
the tools that write remind it to. Nothing runs on the side of the agent.

Claude Code (and the Code tab of Claude Desktop, which reads the same
settings):

    claude mcp add --scope user --transport http kiwi http://127.0.0.1:3456/mcp \
      --header "Authorization: Bearer TOKEN"

### Pages as local files, if you want them

The agents change the body of the pages with `edit_page_body_by_replace`
(exact replacements, as many as needed in one call) and
`write_page_body`, and their header with `write_page_meta`: the
evaluation bench showed nothing that needs more, even a replacement
everywhere in a page. An agent that has a shell can still change the
body of a page as a local file through the API, for instance for a
script on many pages; Kiwi does not propose it. To allow it, put in your
agent guide the recipe and the path of a file (mode 0600) that holds the
line `Authorization: Bearer TOKEN`:

    curl http://localhost:3456/api/page-body/SOURCE/UID?part=whole -sS -o FILE -w '%header{etag}\n' -H @/path/of/the/header/file
    curl http://localhost:3456/api/page-body/SOURCE/UID -sS -X PUT --data-binary @FILE -H @/path/of/the/header/file -H "If-Match: ETAG" --url-query "message=MESSAGE"

- Each command alone, in the current folder (no `cd`, `&&`, `;`, `|`), and
  the header from the file, not from a variable: Claude Code never allows
  in advance a command that holds a variable (`$TOKEN`), nor a compound
  command whose parts are not all allowed. Then this rule, in
  `permissions.allow`, allows them without asking:

      "Bash(curl http://localhost:3456/api/*)"

  The rule only checks the start of the command: Claude Code documents
  that such rules can be bypassed.
- `--data-binary`, never `-d` (it removes the line ends); a 412 answer:
  the body changed meanwhile, download it again. The header is changed
  the same way with `/api/page-meta/SOURCE/UID`. curl 7.87 or later.

### Bridge for the stdio agents

An agent that only runs local programs (MCP on stdio, such as the Chat
tab of Claude Desktop with *claude_desktop_config.json*) goes through
`scripts/kiwi-mcp-bridge.py`, a bridge in Python (standard library
only):

    { "mcpServers": { "kiwi": { "command": "python3",
        "args": ["/path/to/kiwi-mcp-bridge.py", "http://127.0.0.1:3456/mcp"],
        "env": { "KIWI_API_TOKEN": "TOKEN" } } } }

The connectors of claude.ai (and of the Chat tab of Claude Desktop) call
the servers from the cloud of Anthropic: they can not reach a kiwi that
is not public, and are not supported.
