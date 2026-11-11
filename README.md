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

Kiwi is beta software, use at your own risk. Some notes:
- Since it does never write to content files, there is no risk of data
  loss
- The access control system is alpha, if you really need guaranteed
  access control reliability, consider reviewing this code (it is
  short and simple), or getting in touch with the author about it.

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
      - pages
      - images
      - files
  
*content* is the folder that will contain all your *sources*. A
*source* is an independent module of content, with its own pages,
images and files. Kiwi can handle many *sources* in a single database,
and allow to browse their contents together.

You can have as many sources as you want. This can be really useful
for collaboration, aggregation or online publishing.

Your pages should go in the *pages* folder, your images in the
*images* folder and your other files in the *files* folder.

## kiwi.ini config file

This is the main configuration file and it is self documented. It is
read at startup: restart Kiwi after changing it.

`ui-lang` (english or french) only sets the language of the web
interface, see [Full-text search](#full-text-search) for the language
of the pages. Without `editor`, pages can not be opened in an editor
from the web interface (see [Edit file from
browser](#edit-file-from-browser)).

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
    root-tag: botanic
    access: family

* **root-tag** : the tags of the pages of this source are nested under
  this tag, *flower* becomes *botanic > flower*. This keeps the tags
  of a very specific source out of your way when browsing.
* **access** : default access of the pages of this source, comma
  separated groups. It replaces the access of the `[metadata-default]`
  section for these pages, and a page with its own *access* header
  keeps it.
* **lang** : default language of the pages of this source, also used
  for the searches in this source, see [Full-text
  search](#full-text-search). It replaces the lang of the
  `[metadata-default]` section.

All these settings are optional.

## content folder

This folder contains all your *sources*. You can have only one source,
or several. In each source, Kiwi will require the following layout.

### pages

Add your Markdown documents to 'pages' to get started. Kiwi only reads
the files whose name starts with a letter or a digit and ends with
`.md` or `.markdown`: other files (`_draft.md`, `notes.txt`) are
ignored, with a message in debug mode (`kiwi serve --log-level debug`),
as well as hidden files and folders (`.name`). Pages must be UTF-8
files.

Pages filename are used as identifiers, unless you set a custom
identifier in the page: the identifier is the file path from *pages*,
without extension, with `:` between the folders (*recipe/cake.md* gives
`recipe:cake`).

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
* **id** : your page identifier, this is optional, if you don't set it
  kiwi will use the name of the file. In should be unique within the
  page source. It should not include "/" or ":". When two pages of a
  source have the same identifier, only one is used, with a warning.
* **tags** : powerful tags, see below (optional)
* **lang** : language of the content, for full-text search and for the
  `lang` attribute of the page in HTML (optional)
* **access** : access restriction, comma separated allowed groups (optional)
* **archive** : `yes` to archive the page, see below (optional)
* **images-dir** : if you want to include many images in your document
  from one of your images subfolder, set it here, and Kiwi will look
  for your images links from there (optional)
* **files-dir** : if you want to link to many files in your document
  from one of your files subfolder, set it here, and Kiwi will look
  for your files links from there (optional)

Some metadata fields can have default values set in *kiwi.ini*
file. You can change these default values there. These values are then
overridden by page headers. These fields are :

* title
* tags
* access
* lang

When pages are loaded, Kiwi logs a warning for the unknown metadata
keys (a typo, or custom metadata not declared in *kiwi.ini*), and for
the invalid values (`archive: maybe`, an unknown `lang`, a custom
metadata value not matching its type), which are ignored.


**Links format :**

Link to a page, file-name is file path from *pages* without extension
(a `.md` or `.markdown` extension is removed), or the custom
identifier of the page

    [link to a page](<page:File-name>)

Link to a heading of a page, headings get an anchor from their text
(`## Baking time` gives `#baking-time`)

    [link to a section](<page:File-name#baking-time>)
    [link to a section of this page](#baking-time)

Link to an image, file-name is file path from *images*

    [link to an image](<image:flower.png>)

Link to an attachment file, file-name is file path from *files*

    [link to a file](<file:spreadsheet.ods>)

Insert image :

    ![link to an image](<image:flower.png>)

You are free to create subfolders to arrange your files in *pages*,
*images* and *files*. Subfolders have minimal impact on Kiwi
behaviour, however they provide the following properties :

- links to an other page are relative to current page folder. This
  will save you strokes and help you move folders if you need to.
- If meta *images-dir* is not set, links to an image are relative to
  current page subfolder in 'images' folder. In other word, from
  document *pages/recipe/cake.md*, a link to *image:cacao.png* will
  look for file *images/recipe/cacao.png*. If meta *images-dir* is
  set, it takes precedence.
- same applies to files

Links can target path relative to top directories if they start with
*/*, like

    [link to a file](<file:/spreadsheet.ods>)

A custom page identifier does not depend on the folder of its page: from
a subfolder, link to it with `page:/identifier`.

In links, `.` and `..` work as usual (`page:../cake`); a `..` above the
top of the *pages*, *images* or *files* folder is ignored, so a link never
leaves its source. Targets may be percent-encoded, which is needed for
the characters with a meaning in links: `page:C%23` links to the page
*C#.md* (a `#` starts an anchor).

Broken links get the `kiwi-link-broken` class, that the theme shows in
red with a dashed underline. For a reader, a link to a page they can
not read, or between an archived page and a non archived one (see
[Archives](#archives)), is broken too. The *Doctor* page (`/doctor`,
linked at the bottom of every page) lists the pages with broken links,
among those the reader can read, with these links. For admins, it also
lists the files that could not be loaded as pages, with the reason
(invalid metadata header, file that is not UTF-8, page identifier already
used by another file...).


### images and files

You are free to write any type of images and files in dedicated
folders, and to use subfolders if you want to.

**Images and files are public** : they are served to anyone who knows
or guesses their URL (`/image/SOURCE/…`, `/file/SOURCE/…`), logged in
or not, whatever the access of the pages that link to them. Don't put
private content there. Hidden files (`.name`) are not served.

The images and files of a source added while Kiwi runs are served after
a restart (its pages are loaded at the next reload).


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


# Edit file from browser

If you have set your editor in kiwi.ini, you can click on the pen
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
clipboard, so that it is easier for you to link between your
documents. This button is only shown to admins, and the browser only
allows it over HTTPS or on localhost.

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

# Content versioning and backup

Kiwi is unaware of your content history, however you are strongly
encouraged to keep your content in a revision control system.

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
