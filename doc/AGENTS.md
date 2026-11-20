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

The lists of pages (`/api/pages`, `/api/facets`, `/api/links`) choose
their pages with the filters of the browse page (`source`, `tag`, `lang`,
`m.FIELD=VALUE`, `has=FIELD`, `archive=yes`; several `tag` are all
required), `since=YYYY-MM-DD` and `page=SOURCE/UID` (repeated: these pages).

* `GET /api/pages` : the pages, the most recently modified first, at most
  `limit` (100 by default): one `SOURCE/UID` by line, and under it the
  fields of `show` (`show=title,tags,summary,lang,modified`
  and custom fields by their name), written as in a header.
* `GET /api/search?q=QUERY` : full-text search for the agents. The pages
  that contain any of the words, those with the most of them first, each
  one with the words it contains, where (title, tags, content), its
  summary and up to three passages (lines with their section, or
  headings). `"exact phrase"` and `-excluded` words are allowed; `source`,
  `archive=yes` and `limit` (10 by default) too. The search page of the
  web interface is not changed.
* `GET /api/facets` : the tags, langs and custom fields (with their
  description) of the pages selected by the filters of `/api/pages` (all
  the pages without them), by source, each value with its number of
  pages (`show=tags,lang,FIELD`: only these). The agents explore the wiki
  with it as a person does on the browse page: a tag, then another one…,
  and list the pages once there are few of them.
* `GET /api/links` : the links between pages, by source, one by line
  (`UID -> UID`, with `(missing)` when the linked page does not exist or
  can not be read).
* `GET /api/page/SOURCE/UID` : the text of the page, without its uid, and
  the hash of its file as `ETag`. `part=outline` gives its outline (tags,
  summary, backlinks, sections with their lines), `part=section&title=T`
  one section, `part=lines&from=N&to=M` some lines, and `numbered=yes`
  numbers the lines. Everywhere in the API, the lines are those of the
  content, from its first line: the header is not numbered.
* `GET /api/page/SOURCE/UID/meta` : the header of the page.
* `GET /api/folders/SOURCE` (admins) : the folders of the source, with
  their number of pages. The admins also get the path of the file of a
  page in its outline, and of a page they create.
* `GET /api/guide` : the documentation of the API for the agents,
  followed by your guide: the file given by `agent-guide: FILE` in
  *kiwi.ini* (relative to the folder of *kiwi.ini*, or absolute; read
  again at each request; the server does not start if it can not be
  read). It is also given to the MCP agents when they connect. Kiwi has
  no syntax of its own for the requests left to the agents in the pages:
  agree on a convention in your guide, for instance a field
  `status: claude:todo` in the header of the pages to handle (declared as
  a custom metadata), and quotes starting with `AI:` for the requests;
  your guide can also tell the agents to read pages of the wiki.

The writes need the write right on the page, as in the browser editor,
and are committed when the file is in a git repository (author
`LOGIN via Kiwi`; `message=…` adds a message to the one of the commit):

* `PATCH /api/page/SOURCE/UID` : replaces exact passages of the text of
  the page, with a JSON body `{"edits": [{"old": "…", "new": "…"}]}`. Each
  old text must be found exactly once; all the edits apply, or none (409).
* `PUT /api/page/SOURCE/UID` : replaces the whole text, with the hash of
  the page as read in `If-Match` (412 when the page changed since, 428
  without it).
* `POST /api/pages/SOURCE` : creates a page from the text of the body.
  Kiwi gives it a uid, and names its file after its title (` (2)` is added
  when the name is taken); the admins can give its path (`path=folder/name`,
  without extension). The account must have the write right on the new
  page, by the access of the source (an admin can set it in the header).
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

  The syntax is in `GET /api/metadata-guide`, and below the form of the
  metadata editor (`/metadata-editor`, link at the top of the *Metadata*
  page), where the admins run the same scripts: for instance a script
  written by an agent, checked then applied.

## MCP server

Kiwi serves the MCP (Model Context Protocol) of the agents on `POST
/mcp`, with the same API token. Its tools are `search`, `facets`,
`list_pages`, `links`, `read_page`, `folders`, `edit_page`, `write_page`,
`create_page`, `edit_metadata` and `metadata_guide`: each one calls the
API, with the rights of the account. The agent gets the guide of
`/api/guide` when it connects.

An agent connects to it in one of two ways: directly, or through the
bridge, which adds two tools to change a page as a local file. In both
cases, the agent never sees the token, nor the URL of the kiwi.

### Direct connection (HTTP)

The simplest: nothing runs on the side of the agent. The agent has the
tools of Kiwi, and changes a page by replacing passages of its text
(`edit_page`) or by sending it whole (`write_page`). Claude Code (and the
Code tab of Claude Desktop, which reads the same settings):

    claude mcp add --scope user --transport http kiwi http://127.0.0.1:3456/mcp \
      --header "Authorization: Bearer TOKEN"

### Through the bridge (stdio)

`scripts/kiwi-mcp-bridge.py` is a bridge in Python (standard library
only), run by the agent as a local program: it passes the messages of
the agent to `/mcp`, with the token taken from `KIWI_API_TOKEN`. It
serves two cases.

- An agent that has its own tools to edit files (Claude Code): the
  bridge adds two tools. `download_page` writes the text of a page to
  *DIR/SOURCE/UID.md* and answers its path and size, not the text;
  `upload_page` sends the file back (refused if the page changed
  meanwhile: the agent downloads it again and redoes its changes) and
  deletes it. The agent then changes a page with its own tools, without
  reading it all when it is long: the bridge tells it to prefer these
  tools to change a page, and to read a downloaded page whole only under
  20 KB (give another size in your agent guide if you want).

      claude mcp add --scope user kiwi --env KIWI_API_TOKEN=TOKEN -- \
        python3 /path/to/kiwi-mcp-bridge.py http://127.0.0.1:3456/mcp

  *DIR* is given by `--dir DIR` after the URL, by default
  */tmp/kiwi-pages-USER* (created with mode 0700). Add it to the
  `additionalDirectories` of Claude Code, so that it does not ask a
  permission for each file. Give one folder to each kiwi.

- An agent that only runs local programs (MCP on stdio), such as the
  Chat tab of Claude Desktop with *claude_desktop_config.json*:

      { "mcpServers": { "kiwi": { "command": "python3",
          "args": ["/path/to/kiwi-mcp-bridge.py", "http://127.0.0.1:3456/mcp"],
          "env": { "KIWI_API_TOKEN": "TOKEN" } } } }

The connectors of claude.ai (and of the Chat tab of Claude Desktop) call
the servers from the cloud of Anthropic: they can not reach a kiwi that
is not public, and are not supported.
