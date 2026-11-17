# Kiwi

Kiwi is a minimalist content management system, using a database of
Markdown files managed by the user. Kiwi is optimized for text
contents. It aims to offer a powerful solution to handle all types of
notes you want to write for yourself or share with others, and remain
reliable and simple so that you feel comfortable with it.

Kiwi offers some nice features to render, index and browse your
content :
- powerful tags system, with tags-set and sub-tags navigation
- user-defined custom meta-data, with full indexing
- incremental metadata navigation
- full-text search with word stemming available in various languages
- automatic backlinks
- access control
- multi data sources
- a dark theme embedded in the binary, its CSS and JavaScript can be
  changed (see [Development](#development))

Kiwi uses [CommonMark](<https://commonmark.org/>) flavor of Markdown
for contents, with these extensions: tables, footnotes, task lists,
strikethrough, autolinks and heading anchors. The *CommonMark* link at
the bottom of each page (`/commonmark`) shows the syntax. Raw HTML is
allowed, but its scripts are blocked by the Content-Security-Policy of
Kiwi (`script-src 'self'`): only the scripts of the theme run.

Kiwi is beta software, use at your own risk.

# Install from released binaries (Linux)

Static binaries are provided for GNU/Linux x64 systems, you can find
them from https://github.com/PaulRivier/kiwi/releases . Follow the
deployment instructions from there, then come back to this page.

To update it later to the latest release, run `kiwi update` (or
`kiwi update --check` to only check for a new release). It needs
`bash`, `curl`, `tar` and `sha256sum`, and asks for a confirmation
before replacing the binary; the previous one is kept as `kiwi.old`.
Restart your kiwi servers afterwards.

To build Kiwi from source, see [Other install
methods](#other-install-methods) further down.



# Getting started

To get started, create an empty folder where you want to host your
future Kiwi contents. Let's pretend this folder is ~/Kiwi

    mkdir ~/Kiwi

Then run this command to init this folder 

    kiwi init ~/Kiwi

It will populate your folder with directory structure and
configuration file.

In case your folder is not empty, Kiwi will give up, only writing
*kiwi.ini.default* file to help you update your kiwi.ini file.

The theme (css, javascript, images) is embedded in the kiwi binary,
there is nothing to install.

Create your own user account and get admin privileges (this is
important, type 'admin' when prompted for your groups) :

    kiwi access ~/Kiwi/kiwi.ini --create YOUR_NAME

    # This should look like that (the password is not displayed)
    # Password:
    # Password again:
    # Groups (comma separated): admin
    # Account YOUR_NAME created. Running kiwi servers apply the change at
    # the next login or reload (admin reload button).

See [Accounts and access](#accounts-and-access) for more.

You are now ready to start Kiwi :

    kiwi serve ~/Kiwi/kiwi.ini

Open your browser to localhost:3456, and log-in with your account

Kiwi accepts command-line parameters that you can explore using the `--help` switch :

    kiwi --help
    kiwi serve --help
    (...)

`kiwi serve` accepts several config files, to run several kiwis at
once (each one on its own port). It checks them all before starting:
a missing or invalid config file, a missing content directory, or two
kiwis on the same port stop it. When a config file is invalid, a clean
*kiwi.ini.default* is written next to it to help you fix it. Once
started, a server that stops on an error (port already in use for
example) stops the others.

On error, `kiwi serve`, `kiwi init`, `kiwi access` and `kiwi reload` exit with code 1,
`kiwi update` with the code of the update script.


# Kiwi folder layout

In your kiwi folder you should have this layout.

- Kiwi
  - kiwi.ini
  - _accounts (created by `kiwi access`)
  - _pid (while `kiwi serve` runs)
  - _sessions (created at the first login)
  - content
    - my
      - Home.md
      - CommonMark.md
      - recipes
        - cake.md
        - cake__K3M9QW2TZ1.jpg

*content* is the folder that will contain all your *sources*. A
*source* is an independent module of content, with its own pages,
images and files. Kiwi can handle many *sources* in a single database,
and allow to browse their contents together.

You can have as many sources as you want. This can be really useful
for collaboration, aggregation or online publishing.

In a source, you arrange your pages, images and other files as you
like, in any subfolders: an image or a file usually goes next to the
page that links to it, so that moving or removing the folder of a page
takes its files along.

## kiwi.ini config file

This is the main configuration file and it is self documented. It is
read at startup: restart Kiwi after changing it.

`ui-lang` (english or french) only sets the language of the web
interface, see [Full-text search](#full-text-search) for the language
of the pages. Without `editor`, pages can not be opened in your local
editor from the web interface (see [Edit file with your local
editor](#edit-file-with-your-local-editor)); the editor of the browser
does not need it.

`bind` is the address Kiwi listens on. With `bind: 127.0.0.1` (the value
of a new *kiwi.ini*), only the computer running Kiwi can reach it: use it
for a personal Kiwi, or behind a reverse proxy. Without `bind`, Kiwi
listens on all the network interfaces.

If Kiwi is served through HTTPS (behind a reverse proxy for example),
set `secure-cookie: yes` in the `[kiwi]` section: the session cookie
will then only be sent over HTTPS connections. Keep it to `no` if you
access Kiwi with plain HTTP, or login will not work.

Sessions expire after one year, you then have to log in again.

The session cookie is named after the `name` and the `port` of the
kiwi (`kiwi-MyKiwi-3456`), so several kiwis can run on the same host
without mixing their sessions.

### Sources settings

Each source can have its own section in *kiwi.ini*, named after its
folder in *content* :

    [source dad-botanic]
    access: family

* **access** : default access of the pages of this source, comma
  separated groups, with their rights (`family:w`). It replaces the access of the `[metadata-default]`
  section for these pages, and a page with its own *access* header
  keeps it.
* **lang** : default language of the pages of this source, also used
  for the searches in this source, see [Full-text
  search](#full-text-search). It replaces the lang of the
  `[metadata-default]` section.

All these settings are optional.

## content folder

This folder contains all your *sources*. You can have only one source,
or several: each folder of *content* is a source.

### pages

Add your Markdown documents anywhere in the folder of a source to get
started. Kiwi only reads as pages the files whose name starts with a
letter or a digit and ends with `.md` or `.markdown`: other page files
(`-draft.md`) are ignored, with a message in debug mode (`kiwi serve
--log-level debug`), as well as the hidden files and folders (`.name`,
such as `.git`) and those whose name starts with `_` (`_drafts`). Other
files (images…) are not pages: see [images and
files](#images-and-files). Pages must be UTF-8
files, with Unix line endings (LF) and without byte order mark: other
files are not loaded, and are listed on the *Doctor* page.

Each page has a unique identifier in its source, its *uid*, written in
its metadata (`uid: 7KQ2M9XA3B`). Kiwi writes one in the pages that have
none, when it loads them: 10 random characters. It also gives a new uid
to a page whose uid is not valid, or is already the one of another page
of the source (a copied file, for example), and keeps the former one in
its *previous-uids* metadata. The file is then written again, its
metadata header in a normalized form (see [Edit pages in the
browser](#edit-pages-in-the-browser)), and committed when it is in a git
repository (see [Content versioning and backup](#content-versioning-and-backup)).
You may choose a uid yourself (`uid: cake`): from 1 to 64 letters,
digits, `-` or `_`. The address of a page is `/page/SOURCE/UID`, and the
links to a page use its uid: they stay valid when its file is renamed
or moved.

Kiwi documents are composed of metadata and content. Metadata are
fields in the form

    field: value

and must all be at the very top of the file, without any blank line.

Document content starts after first empty line. In the metadata, a line
is a field (`field: value`, the field name is a single word of letters,
digits, `-` and `_`, or the name of one of your custom metadata) or a
separator made of `-` or `.` (`---`, `...`), so for example all this forms
of metadata headers are acceptable

```
title: The title
tags: tag1

# My document
...
```

```
title: The title
tags: tag1
...

# My document
...
```

```
---
title: The title
tags: tag1
---

# My document
...
```

But these are NOT acceptable, because of the lack of newlines, or
because of undue presence of ':'. A page whose first block is anything
else than metadata (a heading, some text) is an error: the page is not
loaded, a warning is logged, and its file is listed on the *Doctor* page.
A page without metadata starts with an empty line.

```
title: The title
tags: tag1
# My document
...
```

```
title: The title
tags: tag1
:::

# My document
...
```

Metadata should include at least a 'title' field in the header. See
Home.md for file format example.

Some metadata keys are reserved by kiwi :

* **title** : title of your page (optional, but a page without title
  gets the default one, *untitled*)
* **summary** : a short description of the page, on one line
  (optional). It is shown when the mouse is over a link to the page.
* **uid** : the identifier of the page, unique in its source (see
  above). Kiwi writes it when it is missing.
* **previous-uids** : the uids that Kiwi replaced on this page,
  separated by commas. Kept by Kiwi, don't change it.
* **tags** : powerful tags, see below (optional)
* **lang** : language of the content, for full-text search and for the
  `lang` attribute of the page in HTML (optional)
* **access** : access restriction, comma separated allowed groups,
  with their rights, see [Accounts and access](#accounts-and-access)
  (optional)
* **archive** : `yes` to archive the page, see below (optional)

Some metadata fields can have default values set in *kiwi.ini*
file. You can change these default values there. These values are then
overridden by page headers. These fields are :

* title
* tags
* access
* lang

When pages are loaded, Kiwi logs a warning for the unknown metadata
keys (a typo, or custom metadata not declared in *kiwi.ini*), and for
the invalid values (`archive: maybe`, an unknown `lang`, an invalid
*access* group, a custom metadata value not matching its type), which
are ignored.


**Links format :**

Link to a page of the same source, by its uid (the chainlink button of
a page copies a link to it; spaces around the uid are allowed:
`<page: cake>`). A page can not link to a page of another source.

    [link to a page](page:UID)

Link to a heading of a page, headings get an anchor from their text
(`## Baking time` gives `#baking-time`)

    [link to a section](page:UID#baking-time)
    [link to a section of this page](#baking-time)

Link to a file of the source (an image, a spreadsheet…) by its uid
(see [images and files](#images-and-files)): the link stays valid when
the file is renamed or moved in the source. A link to an image opens it
in the image viewer of the page, a link to another file in a new tab.

    [link to an image](file:K3M9QW2TZ1)
    [link to a file](file:XJD54PM77T)
    [link to a page of a PDF](file:XJD54PM77T#page=2)

Insert an image:

    ![a cake](file:K3M9QW2TZ1)

A file link whose target is not the uid of a file of the source is
broken.

Broken links get the `kiwi-link-broken` class, that the theme shows in
red with a dashed underline. For a reader, a link to a page they can
not read, or between an archived page and a non archived one (see
[Archives](#archives)), is broken too. The *Doctor* page (`/doctor`,
linked at the bottom of every page) lists the pages with broken links,
among those the reader can read, with these links (`file:` followed
by the target for a missing file). For admins, it also lists the files
that could not be loaded, with the reason (invalid metadata header,
file that is not UTF-8, file where Kiwi could not write the uid of the
page, file that Kiwi could not rename to give it a uid...), and the
files that no page links, archived pages included (Kiwi does not serve
them), each one with a button that copies a link to it
(`[cake.jpg](file:K3M9QW2TZ1)`).


### images and files

Images and other files go anywhere in the folder of the source, next to
the pages or in subfolders, as the pages.

Each file gets a uid, unique in its source, written in its name: when
Kiwi loads a file without one, it renames it, `cake.jpg` becoming
`cake__K3M9QW2TZ1.jpg` (10 characters of Crockford base32 after `__`,
before the first `.` of the name). The uid is read in the name: `__`,
then 10 upper case characters of the Crockford base32 alphabet, then
the end of the name or a character that is neither a letter nor a
digit. A file can then be renamed or moved anywhere in its source, as
long as it keeps this `__UID` part in its name: its links still work.
A copy of a file (`cake__K3M9QW2TZ1 (copy).jpg`) has the same uid: the
file already known keeps it (else the first one by path), the other
one gets a new uid, written in place of the former. The renamed files
are committed, as the pages whose uid is written (see [content
versioning](#content-versioning-and-backup)), first as they are, then
renamed.

Kiwi serves a file (`/file/SOURCE/UID`, with its name without its uid)
only when a page links to it with a `file:` link, and only to the
readers of at least one of the pages that link to it (archived or
not): a file linked by a private page only is private, a file linked by
no page is never served. A reader who can not read any of these pages
gets the same answer as for a page (401, or 403 when logged in). Links
written in raw HTML (`<img src="/file/…">`) do not count. As a
consequence, a writer of a page can make any file of the source
readable by the readers of this page, by linking to it.

Kiwi never gives a uid to, nor serves, these files, nor anything in a
folder whose name is one of those:

- the files of the pages (`.md`, `.markdown`) and the copies that
  editors leave next to them (`page.md~`, `#page.md#`, `page.md.orig`…:
  a name holding `.md` or `.markdown` followed by anything but a letter
  or a digit);
- the names that start with `.`, `_`, `#` or `~`, or end with `#` or
  `~` (temporary files of editors, of Office, of syncthing on Windows…);
- the conflict copies of syncthing (a name holding `.sync-conflict-`);
- the temporary and backup files, by their extension, in any case:
  `tmp`, `temp`, `part`, `partial`, `crdownload`, `download`, `swp`,
  `swo`, `bak`, `orig`, `rej`, `lock`, `lck`, `kiwi-migration`;
- `Thumbs.db` and `desktop.ini`.

These files are listed in debug mode (`kiwi serve --log-level debug`).
The text files are served as UTF-8.

A file added to the disk gets its uid, and is served, once the pages
are reloaded.


# Documents reload

Documents are reloaded on demand. Logged in as an admin, click on the
reload button of the web interface: Kiwi reloads the new, modified,
renamed and removed pages, reads the accounts file again, then your
browser page is reloaded. If it fails (session expired, not an admin),
a message tells why.

The same reload can be asked from the command line, on the machine running
Kiwi (from a script, a git hook, a cron job...):

    kiwi reload ~/Kiwi/kiwi.ini

It sends a signal to the server of this kiwi, found by the *_pid* file that
`kiwi serve` writes next to *kiwi.ini*. The command does not wait for the
reload: its result is in the logs of the server. A server running several
kiwis reloads them all.

Changes to *kiwi.ini* need a restart.

# Tags

Tags are a powerful mean to organise information. With Kiwi, you are
encouraged to use tags on your documents. In the header of your files,
write for example :

    tags: tag1, tag2

That's it.

Kiwi also has a nice subtags mechanism, to help you organize better
your content and control tags proliferation. Use it as demonstrated
below :

    title: Japanese cake
    tags: cooking > recipe > cake, favorite, area > Japan

This will just work as you want it to work.

If you prefer you can span tags on multiple lines :

    tags: cooking > recipe > cake
    tags: favorite
    tags: area > Japan

Subtags sharing the same parent can be grouped between braces :

    tags: sport > {climbing, hiking}, area > {Alps, Pyrenees}

is the same as :

    tags: sport > climbing, sport > hiking, area > Alps, area > Pyrenees

Groups can be nested, and placed anywhere in a tag :

    tags: sport > {mountain > {climbing, hiking}, swimming}
    tags: {sport, leisure} > mountain

Also if you need to include a comma or a brace in your tag, escape it with a
backslash : `\,`, `\{` or `\}`. This is discouraged though.

Browsing files by tags should be intuitive from the web interface,
look for the button with labels. Don't forget to reload when you want.

# Custom metadata

You can use your own custom metadata. Declare them first in your *kiwi.ini*
file, with an entry similar to the following.


    [metadata category]
    type: text
    threshold: 0.8

    [metadata authors]
    type: text
    threshold: 0.5

`threshold` defines when a custom filter for this metadata key will
show in the browsing interface. At `0.8`, it will appear as soon as
the number of pages having this metadata is more than 80% of the total
number of pages matching the current browsing filters.

Then restart Kiwi. You can use them in metadata section of pages like
so :

    category: Places
    authors: John, Jack

Custom metadata type can be `text`, `int`, `date` or `bool`. Values
are comma separated. Date format is "YYYY-MM-DD", bool values are
`yes`, `true`, `1`, `no`, `false` or `0`. A value that does not match
the type is ignored, with a warning in the logs.


# Archives

A page with `archive: yes` (or `true`, `1`) in its header is archived : it
is left out of the normal navigation (browsing, search, backlinks, metadata).
The "Archives" switch, at the right of the sources on the browse page, goes
to the archives : you then only navigate in the archived pages, the switch
is red, and a red "Archives" label is shown at the top of pages and search
results. Both navigations are separated : a link from a page to a page of
the other one is broken. Access restrictions apply to archives as usual.


# Full-text search

Documents are indexed with a full-text search engine that is
natural-language-aware. If your document is written in default
language, as set in *kiwi.ini* metadata configuration, you don't have
anything to add.

Otherwise, you will need to set the *lang* header to your documents
metadata. Acceptable languages are danish, dutch, english, finnish,
french, german, hungarian, italian, norwegian, portuguese, romanian,
russian, spanish, swedish and turkish.


    title: French fries : les Frites
    tags: cooking > recipe, area > France
    lang: french

Kiwi can also detect the language from the content of the pages, for
French, English and Spanish. Set it in the `[metadata-default]`
section of *kiwi.ini*, or in the settings of a source, with `auto`
followed by the language to use when the detection fails (too short or
mixed content, other language) :

    lang: auto/french

`auto` alone is the same as `auto/english`.

The search terms are processed in the language of the searched
sources (those selected in the navigation, or all of them): the `lang`
of their `[source …]` section, or else the one of the
`[metadata-default]` section, `auto/X` counting as X. When these
sources don't share a language, the one of the `[metadata-default]`
section is used. Inflected forms work best for pages in that language.
To search in another language, start your search with its code :

    es: canciones

Codes are da (danish), nl (dutch), en (english), fi (finnish), fr
(french), de (german), hu (hungarian), it (italian), no (norwegian),
pt (portuguese), ro (romanian), ru (russian), es (spanish), sv
(swedish) and tr (turkish). The results page shows the language used,
as in "Query [spanish]: es: canciones".

Pages and queries are split into words the same way, on spaces,
punctuation (apostrophes, hyphens, quotes…) and symbols: `chocolate-banana`
searches the words *chocolate* and *banana*. Case and accents don't
matter.

## Search by uid

A search that starts with `uid:` is not a full-text search: `uid:UID`
shows the page with this uid, the file with this uid (the one of its
name, see [images and files](#images-and-files)), and the pages that
link to this uid, with a `page:` or a `file:` link. It searches the
sources selected in the navigation (all of them if none), among the
archived pages or not as the navigation, and only the pages that you
can read:

    uid:K3M9QW2TZ1


# Edit pages in the browser

The users with the write right on a page (see [Accounts and
access](#accounts-and-access)) see a pen in the navigation bar: it
opens the page in a markdown editor, with a toolbar, a preview and a
side by side view. On a computer, a small pen also appears in the right
margin of the block under the mouse: it opens the editor at the line of
this block.

The editor shows the whole file, its metadata header included, written
in a normalized form: one line by key, in a fixed order (title, uid,
previous-uids, tags, access, lang, archive, the
custom metadata, then the other keys), the tags sorted. The *uid* and
*previous-uids* metadata are never shown, and are kept as they are in
the file when the page is saved, whatever the right. With the write
right, the *access* and *archive* metadata
are not shown either, and are kept the same way; with the admin right,
they can be changed.

When the page is saved, Kiwi writes the file (atomically, with its
former permissions), and reloads the pages. The save is refused when
the file has changed since the editor was opened (by another editor or
another user): keep a copy of your text, and open the editor again. It
is also refused when the page could not be loaded (an invalid header).
Until it is saved, the
text is kept as a draft in your browser, for this version of the file.

When the saved file is in a git repository, Kiwi commits it: see
[Content versioning and backup](#content-versioning-and-backup).

The save and preview requests from the pages of another site are
refused: behind a reverse proxy, keep the `Host` header of the requests
(or set `X-Forwarded-Host`).

# Edit file with your local editor

If you have set your editor in kiwi.ini, you can click on the open
picture to open your document in your editor. This button is only shown
to admins, and only when `editor` is set.

The editor is started by the Kiwi server, on the machine where it runs:
this is meant for a Kiwi running on your own computer. The `editor`
command is split on spaces, without any quoting, and the file path is
added as last argument; use a wrapper script for a path with spaces.
Your browser page is reloaded at once: click on the reload button once
the file is saved to see the changes.

# Copy link

Clicking on the chain link will copy a link to current page to your
clipboard (`[Title](page:UID)`), so that it is easier for you to link
between the documents of a source. This button is only shown to admins and to the users with the
write right on the page, and the browser only allows it over HTTPS or on
localhost.

# Accounts and access

Accounts are managed with `kiwi access`, and stored in the *_accounts*
file next to *kiwi.ini* :

    kiwi access ~/Kiwi/kiwi.ini --create LOGIN
    kiwi access ~/Kiwi/kiwi.ini --update LOGIN   # new password or groups
    kiwi access ~/Kiwi/kiwi.ini --remove LOGIN

Each line of *_accounts* starts with the version of its format, currently
`1:login:salt:hash:groups`. A line without a known version is ignored, with
a warning in the logs of the servers, and `kiwi access` refuses to change
the file until it is fixed or removed.

A login is made of letters, digits, `-` and `_`. With `--update`, an
empty answer keeps the current password or groups. Running servers read
the accounts again at each login and at each reload: there is no need
to restart them. A new password, or a removed account, closes the
sessions of the account once the servers have read the accounts again.

Kiwi does not limit the login attempts itself. A refused login is a
`POST /login` answered with the status 401 (a successful one is a
redirection, 302): use it to limit them in your reverse proxy, or with
fail2ban on the access logs of the proxy. Behind a proxy, the logs of Kiwi
(`login failed from ADDRESS`) only show the address of the proxy.

Each page is readable by the groups of its *access* metadata. The
`public` group means everyone, logged in or not, and members of the
`admin` group read all the pages. The default access of the pages is
set in the `[metadata-default]` section, or in the settings of their
source; without it, it is `admin`.

A group can also get a right on the page, after a colon :

    access: me:a, family:w, friends, public

* **r** (the default, `friends` is `friends:r`) : read the page ;
* **w** (or **rw**) : read and edit the page in the browser, its
  content and its metadata, except *access* and *archive* ;
* **a** : read and edit everything in the page, these metadata included
  (but not *uid* and *previous-uids*, kept by Kiwi).

`public` can only read. A group given several times gets its highest
right. An invalid item (`family:x`, `public:w`) gives no right at all,
not even to read, and is logged as an invalid value when the pages are
loaded; in *kiwi.ini*, it is an error. Members of the `admin` group have every
right on every page.

# API for the agents

Kiwi has an HTTP API under `/api/`, made for the AI agents (Claude…) and
the scripts: its answers are compact texts. A request needs an API token
of an account, and gets the rights of this account:

    kiwi access ~/Kiwi/kiwi.ini --token LOGIN           # displayed once
    kiwi access ~/Kiwi/kiwi.ini --revoke-tokens LOGIN
    curl -H "Authorization: Bearer TOKEN" http://localhost:3456/api/pages

The tokens are kept hashed in the *_tokens* file, next to *kiwi.ini*. A
token stops working when its account is removed or gets a new password.

* `GET /api/pages` : the pages, the most recently modified first, one by
  line (`SOURCE/UID`, day, title, tags, then summary), filtered as on the
  browse page (`source`, `tag`, `lang`, `m.FIELD`, `has`, `archive=yes`),
  with `since=YYYY-MM-DD` and `limit` (100 by default).
* `GET /api/search?q=QUERY` : full-text search for the agents. The pages
  that contain all the words come first (alone when there are some), each
  one with the words it contains, where (title, tags, content), and up to
  three passages (lines with their section, or headings). `"exact phrase"` and
  `-excluded` words are allowed; `source`, `archive=yes` and `limit` (10 by
  default) too. The search page of the web interface is not changed.
* `GET /api/vocabulary` : the tags, langs and custom fields of each
  source (`source` to choose them), with their number of pages.
* `GET /api/page/SOURCE/UID` : the text of the page, without its uid, and
  the hash of its file as `ETag`. `part=outline` gives its outline (tags,
  summary, backlinks, sections with their lines), `part=section&title=T`
  one section, `part=lines&from=N&to=M` some lines, and `numbered=yes`
  numbers the lines.
* `GET /api/page/SOURCE/UID/meta` : the header of the page.
* `GET /api/folders/SOURCE` (admins) : the folders of the source, with
  their number of pages. The admins also get the path of the files in the
  lists.
* `GET /api/guide` : the guide of kiwi for the agents, followed by yours:
  the page given by `agent-guide: SOURCE/UID` in *kiwi.ini*.

The writes need the write right on the page, as in the browser editor,
and are committed when the file is in a git repository (author
`LOGIN via Kiwi`):

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
* `POST /api/metadata/SOURCE` : changes the headers of the pages of a
  source with a script, one command by line, all of them or none, in one
  commit (a command on all the pages that changes none is an error):

      # on all the pages of the source
      meta tags move "cuisine > matériel" "matériel > cuisine"
      meta tags delete obsolete
      meta ingredients move bananas banana
      meta ingredients delete salt
      meta author rename-to writer
      # on one page, by its uid
      page 0123456789 meta summary set "First aid: wounds, burns"
      page 0123456789 meta tags add health
      page 0123456789 meta tags remove todo
      page 0123456789 meta lang unset

  A tag moves or goes with its sub-tags. Changing `access` needs the
  admin right on the page.

## MCP server for the agents

`kiwi mcp` is an MCP server (Model Context Protocol) on stdio, for the
agents that can run one (Claude Code, Claude Desktop…). It is a client of
the API of a running kiwi: it keeps nothing, the kiwi server reads and
writes the pages. Its tools are `search`, `list_pages`, `vocabulary`,
`read_page`, `folders`, `edit_page`, `write_page`, `create_page` and
`edit_metadata`, and it
gives the guide of `/api/guide` to the
agent when it connects. The token is read from the `KIWI_API_TOKEN`
environment variable, or from a file (`--token-file`):

    claude mcp add kiwi -e KIWI_API_TOKEN=TOKEN -- kiwi mcp --url http://127.0.0.1:3456

For Claude Desktop, in *claude_desktop_config.json* :

    { "mcpServers": { "kiwi": { "command": "/path/to/kiwi",
                                "args": ["mcp", "--url", "http://127.0.0.1:3456"],
                                "env": { "KIWI_API_TOKEN": "TOKEN" } } } }

`kiwi mcp` speaks plain HTTP only: give it the local address of the kiwi.

# Content versioning and backup

You are strongly encouraged to keep your content in a git repository
(one by source, the whole content folder, or the kiwi folder).

When a page saved from the browser is in a git repository, and not
ignored there (`.gitignore`), Kiwi commits this file alone: a file not
tracked yet is added, and what you have staged for other files stays
staged, out of the commit. The author of the commit is *LOGIN via
Kiwi* (without email), its committer is *Kiwi*, and its message is
`Edit SOURCE:UID (kiwi NAME)`. When the file differed from its last
commit before the save (changes not committed, or a file not tracked
yet), Kiwi first commits it as it was, with *Kiwi* as author and the
message `Pre-update commit of SOURCE:UID (kiwi NAME)`: the changes made
outside of Kiwi are not attributed to the user of the browser.

Kiwi commits the files it writes when it loads the pages (the uids it
writes, see [pages](#pages)) the same way: one commit by repository,
with *Kiwi* as author and the message `Kiwi: uid assigned to N pages`,
after a commit `Kiwi: pre-update commit of N pages` of those that
differed from their last commit. The files of the sources that it
renames to give them a uid (see [images and files](#images-and-files))
are committed the same way, with the messages `Kiwi: pre-update commit
of N files` and `Kiwi: uid assigned to N files`: git sees the renames.

Set `git-commit: no` in the `[kiwi]` section of kiwi.ini to disable these
commits. Kiwi does not push, and the files changed with your local
editor are not committed.

The commit is made with the `git` program; when it fails (git not
installed, a hook that refuses the commit, a merge in progress, more
than 60 seconds), the page is saved anyway and the error is logged. If
Kiwi runs as another user than the owner of the repository, git refuses
to work in it until the repository is added to the `safe.directory`
setting of this user, for example
`git config --global --add safe.directory /path/to/repository`.

Don't put *_accounts* and *_sessions* under version control: they hold
the password hashes and the sessions. Kiwi writes them readable by
their owner only.


# Other install methods

For released binaries, see [Install from released binaries
(Linux)](#install-from-released-binaries-linux).

## From source


### podman way (easy)

This is for GNU/Linux x64 builds, based on Alpine/musl/ghc with native
integers.

From the project directory, run :

    ./podman-build.sh
    
It should build the static binary in the `release` directory.

### Cabal way

Get the source code, check GHC compiler version from head of file `cabal.project`, and install it for example with `ghcup`.

Then, from source directory, run as user :

    cabal install

It uses the `cabal.project` file (optimized build). The other project
files are for development (`cabal.project.devel`, not optimized) and
for static release builds (`cabal.project.musl`).

You may need to install the following packages on your system :

- libgmp-dev
- zlib1g-dev
- pkg-config


# Development

The theme files (css, js, images) are embedded in the binary: a changed theme needs a new build. To work on them without building again, set `KIWI_THEME_DIR` to the theme folder when running kiwi : the files are then read from this folder at each request, and a page reload shows the changes.

    KIWI_THEME_DIR=static/themes/dark kiwi serve path/to/kiwi.ini


# License

Copyright (C) 2020 - Paul Rivier

Kiwi is free software, released under the GNU General Public License,
version 3 or later. See the *LICENSE* file.
