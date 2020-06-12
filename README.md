# Kiwi

Kiwi is a minimalist content management system, using a database of
Markdown files managed by the user. Kiwi is optmized for text
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

At this stage Kiwi is alpha software, use it with caution.

# Install

## From released binaries

Binaries are provided for GNU/Linux x64 systems, you can find them
from https://github.com/PaulRivier/kiwi/releases . Follow the
deployment instructions from there, then come back to this page.

## From source

Get the source code, open a terminal into this folder then run as
user :

    stack install

Hopefully the build will complete successfully, and install a binary
in your local bin folder.

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

At this step, you should symlink the bundled 'themes' folder to your
kiwi folder. Alternatively, edit your *kiwi.ini* to point "themes-dir"
to the theme you want in the bundled themes folder. Themes are
separated from code, therefore they are very easy to add to Kiwi,
check how existing themes are defined.

    cd ~/Kiwi
    ln -s ~/PATH-TO-KIWI-GIT-CLONE/themes themes

Create your own user account and get admin privileges (this is
important, type 'admin' when prompted for your groups) :

    kiwi access ~/Kiwi/kiwi.ini --create YOUR_NAME

    # This should look like that
    # Password: ********
    # Groups (comma separated):
    # admin
    # Access created


You are ready to start Kiwi :

    kiwi serve ~/Kiwi/kiwi.ini

Open your browser to localhost:3456, and log-in with your account

# Kiwi folder layout

In your kiwi folder you should have this layout.

- Kiwi
  - kiwi.ini
  - content
    - my 
      - pages
      - images
      - files
  - themes
  
*content* is the folder that will contain all your *sources*. A
*source* is an independent module of content, with its own pages,
images and files. Kiwi can handle many *sources* in a single database,
and allow to browse their contents together.

You can have as many sources as you want. This can be really useful
for collaboration, aggregation or online publishing.

Your pages should go in the *pages* folder, your images in the
*images* folder and your other files in the *files* folder.

## kiwi.ini config file

This is the main configuration file and it is self documented. Again,
be sure that you theme-dir configuration is pointing to a real theme
directory. Start with one bundled with this repository.

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
  document *pages/receipe/cake.md*, a link to *image:cacao.png* will
  look for file *images/receipe/cacao.png*. If meta *images-dir* is
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
    tags: cooking > receipt > cake, favorite, area > Japan

This will just work as you want it to work.

If you prefer you can span tags on multiple lines :

    tags: cooking > receipt > cake
    tags: favorite
    tags: area > Japan

Also if you want to include a comma in your tag, escape it with a
backslash '\,'.

Browsing files by tags should be intuitive from the web interface,
look for the button with labels. Don't forget to reload when you want.

# Custom metadata

You can use your own custom metadata. Set them in your *kiwi.ini*
file, and restart Kiwi. Then you can use them like so :

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
    tags: cooking > receipt, area > France
	lang: french


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
