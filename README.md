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
- clear view/model separation, highly themable (html + CSS + JS)

Kiwi uses [CommonMark](<https://commonmark.org/>) flavor of Markdown
for contents.

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

To update it later to the latest release, run `kiwi update` (or `kiwi
update --check` to only check for a new release). It asks for a
confirmation before replacing the binary; the previous one is kept as
`kiwi.old`. Restart your kiwi servers afterwards.

For more installation options, see "Other install methods" further
down.



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

    # This should look like that
    # Password: ********
    # Groups (comma separated):
    # admin
    # Access created


You are now ready to start Kiwi :

    kiwi serve ~/Kiwi/kiwi.ini

Open your browser to localhost:3456, and log-in with your account

Kiwi accepts command-line parameters that you can explore using the `--help` switch :

    kiwi --help
    kiwi serve --help
    (...)


# Kiwi folder layout

In your kiwi folder you should have this layout.

- Kiwi
  - kiwi.ini
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

This is the main configuration file and it is self documented.

If Kiwi is served through HTTPS (behind a reverse proxy for example),
set `secure-cookie: yes` in the `[kiwi]` section: the session cookie
will then only be sent over HTTPS connections. Keep it to `no` if you
access Kiwi with plain HTTP, or login will not work.

Sessions expire after one year, you then have to log in again.

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
* **lang** : default language of the pages of this source, see
  [Full-text search](#full-text-search). It replaces the lang of the
  `[metadata-default]` section.

All these settings are optional.

## content folder

This folder contains all your *sources*. You can have only one source,
or several. In each source, Kiwi will require the following layout.

### pages

Add your Markdown documents to 'pages' to get started. Pages filename
are used as identifiers, unless you set a custom identifier in the
page.

Kiwi documents are composed of metadata and content. Metadata are
fields in the form

    field: value

and must all be at the very top of the file, without any blank line.

Document content starts after first empty line. Lines in metadata that
don't include ':' are ignored, so for example all this forms of
metadata headers are acceptable

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
because of undue presence of ':'.

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

* **title** : title of your page (required)
* **id** : your page identifier, this is optional, if you don't set it
  kiwi will use the name of the file. In should be unique within the
  page source. It should not include "/" or ":".
* **tags** : powerful tags, see below (optional)
* **lang** : language of the content, for full-text search (optional)
* **access** : access restriction, comma separated allowed groups (optional)
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

You are free to use any filename extension for your pages, as long as
the format inside is metadata header + blank line + CommonMark
Markdown.


**Links format :**

Link to a page, file-name is file path from *pages* without extension

    [link to a page](<page:File-name>)

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


### images and files

You are free to write any type of images and files in dedicated
folders, and to use subfolders if you want to.


# Documents reload

Documents are reloaded on demand. From the web interface, click on the
reload button. This will reload the database, not your browser
page. You may need to hit your browser reload button as well.

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
backslash : '\,', '\{' or '\}'.

Browsing files by tags should be intuitive from the web interface,
look for the button with labels. Don't forget to reload when you want.

# Custom metadata

You can use your own custom metadata. Declare them first in your *kiwi.ini*
file, with an entry similar to the following.


    [metadata category]
    type: text
    threshold: 0.8

    [metadata author]
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
    
Custom metadata type can be text, integer, date or bool. Date format
is "YYYY-MM-DD".


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

The search terms are processed in the language of the
`[metadata-default]` section (its fallback language with `auto`), so
inflected forms work best for pages in that language. To search in
another language, start your search with its code :

    es: canciones

Codes are da (danish), nl (dutch), en (english), fi (finnish), fr
(french), de (german), hu (hungarian), it (italian), no (norwegian),
pt (portuguese), ro (romanian), ru (russian), es (spanish), sv
(swedish) and tr (turkish).


# Edit file from browser

If you have set your editor in kiwi.ini, you can click on the pen
picture to open your document in your editor.

# Copy link

Clicking on the chain link will copy a link to current page to your
clipboard, so that it is easier for you to link between your
documents.

# Content versioning and backup

Kiwi is unaware of your content history, however you are strongly
encouraged to keep your content in a revision control system.


# Other install methods

## From released binaries

Static binaries are provided for GNU/Linux x64 systems, you can find
them from https://github.com/PaulRivier/kiwi/releases . Follow the
deployment instructions from there, then come back to this page.

To update it later to the latest release, run `kiwi update` (or
`kiwi update --check` to only check for a new release). It needs
`bash`, `curl`, `tar` and `sha256sum`, and asks for a confirmation
before replacing the binary; the previous one is kept as `kiwi.old`.
Restart your kiwi servers afterwards.

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

    cabal install --project-file=cabal.project.devel

You may need to install the following packages on your system :

- libgmp-dev
- zlib1g-dev
- pkg-config


# Development

The theme files (css, js, images) are embedded in the binary. To work on them without building again, set `KIWI_THEME_DIR` to the theme folder when running kiwi : the files are then read from this folder at each request, and a page reload shows the changes.

    KIWI_THEME_DIR=static/themes/dark kiwi serve path/to/kiwi.ini


# License

Copyright (C) 2020 - Paul Rivier

Kiwi is free software, released under the GNU General Public License,
version 3 or later. See the *LICENSE* file.
