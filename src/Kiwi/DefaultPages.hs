{-# LANGUAGE QuasiQuotes #-}

module Kiwi.DefaultPages where

import           Text.RawString.QQ (r)


defaultHomePage :: String
defaultHomePage =
  [r|title: Kiwi !
tags: kiwi, help
---

# Kiwi !

- Welcome
- Bienvenue
- Bienvenido
- Benvenuto

# What to do now

Make sure you read the README file.

[You can also read the CommonMark Page](page:CommonMark)

This page is yours. You can edit it and write what you want here.
|]


commonmarkDocPage :: String
commonmarkDocPage =
  [r|title: CommonMark cheatsheet
tags: kiwi, help, CommonMark
lang: english
~~~

This is Kiwi cheatsheet for [commonmark](https://commonmark.org/). You
can also have a look at the [commonmark tutorial](https://commonmark.org/help/).


# Standard features

## Headings

Headings from `h1` through `h6` are constructed with a `#` for each level:

```
# h1 Heading
## h2 Heading
### h3 Heading
#### h4 Heading
##### h5 Heading
###### h6 Heading
```

## Paragraphs

Paragraphs are defined as plain text blocks separated by blank
lines. The number of blank lines between paragraphs in your CommonMark
document has no effect on the output document.

```
Lorem ipsum dolor sit amet, graecis denique ei vel, at duo primis
mandamus. Et legere ocurreret pri, animal tacimates complectitur ad
cum.

Cu eum inermis inimicus efficiendi. Labore officiis his ex,
soluta officiis concludaturque ei qui, vide sensibus vim ad.
```

## Force blank lines

If you really need to insert extra newlines in the output, you can use
`<br>` elements. They will translate to blank lines in the output.


## Horizontal Rule

In CommonMark, you can use either of the following to create a
horizontal rule:

* `___` : three consecutive underscores
* `---` : three consecutive dashes
* `***` : three consecutive asterisks


## Emphasis

### Bold or emphasis

Emphasize your words with double asterisks.

```
The following snippet of text is **emphasized with double asterisks**
```

→ The following snippet of text is **emphasized with double asterisks**.


### Italics

For emphasizing a snippet of text with italics.


```
The following snippet of text is _rendered as italicized text_.
```

→ The following snippet of text is _rendered as italicized text_.

## Strikethrough

In addition, you can do strickthrough.

```
~~Strike through this text.~
```

→ ~~Strike through this text.~~




## Lists

### Unordered

A list of items without any numbering.

You may use any of the following symbols to denote bullets for each
list item:

```
* valid bullet
- valid bullet
+ valid bullet
```

For example

```
- first item
- second item
  - sub-items are allowed
  - by indenting two spaces
- third item
```

→

- first item
- second item
  - sub-items are allowed
  - by indenting two spaces
- third item



### Ordered

A list of items in which the order of items does explicitly matter.

```
1. first item
2. second item
3. third item
```

→

1. first item
2. second item
3. third item

Numbering in your document is optional, CommonMark will number the
output automatically.

```
1. first item
1. second item
1. third item
```

→

1. first item
1. second item
1. third item


### Todo List

You can also render Todo lists

```
- [X] Lorem ipsum dolor sit amet
- [ ] Consectetur adipiscing elit
- [ ] Integer molestie lorem at massa
```

→

- [X] Lorem ipsum dolor sit amet
- [ ] Consectetur adipiscing elit
- [ ] Integer molestie lorem at massa


## Blockquotes

Generaly used for a section of quoting text from another source,
within your document.

To create a blockquote, use `>` before any text you want to quote.

```
> Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer
> posuere erat a ante
```

→

> Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer
> posuere erat a ante.


It is possible to nest several levels of blockquote by adding '>' in
front of the line.


## Code

### Inline code

Wrap inline snippets of code with a single backtick: <code>`</code>.

For example, to show `<div></div>` inline with other text, just wrap
it in backticks.

```html
For example, to show `<div></div>` inline with other text, just wrap it in backticks.
```

### "Fenced" code block

Three consecutive backticks, referred to as "code fences", are used to
denote multiple lines of code: <code>```</code>.

For example, this:

<pre>
```html
Example text here...
```
</pre>

Appears like this when viewed in a browser:

```
Example text here...
```

### Indented code

You may also indent several lines of code by at least four spaces, but this is not recommended as it is harder to read, harder to maintain, and doesn't support syntax highlighting.

Example:

```
    // Some comments
    line 1 of code
    line 2 of code
    line 3 of code
```

→ 

    // Some comments
    line 1 of code
    line 2 of code
    line 3 of code



## Links

### Autolinks

Autolinks are absolute URIs and email addresses inside `<` and
`>`. They are parsed as links, where the URI or email address itself
is used as the link's label.

```
<http://foo.bar.baz>
```

→ <http://foo.bar.baz>

URIs or email addresses that are not wrapped in angle brackets are not
recognized as valid autolinks by markdown parsers.


### Inline links

Make a link to an other Kiwi document from your document. Syntax is
`[text of the link](<page:ID>)` where ID is the file name, from your
pages directory, without file extension. For example to link to your
Home.md file.

```
[Home page](<page:Home>)
```

→ [Home page](<page:Home>)


Or make a link to an external page.

```
[Cheesecake](<https://en.wikipedia.org/wiki/Cheesecake>)
```

→ [Cheesecake](<https://en.wikipedia.org/wiki/Cheesecake>)

Or make a link to one of your files

```
[My best spreadsheet](<file:my-spreadsheet.xls>)

[This flower](<image:flower.jpg>)
```


## Images

Images have a similar syntax to links but include a preceding exclamation point.

```
![Flower](image:flower.png)
```


## Raw HTML

Any text between `<` and `>` that looks like an HTML tag will be
parsed as a raw HTML tag and rendered to HTML without escaping.


Example:

```
**Visit <a href="https://www.wikipedia.org">Wikipedia page</a>.**
```

→ **Visit <a href="https://www.wikipedia.org">Wikipedia page</a>.**


## Escaping with backslashes

Any ASCII punctuation character that you want to insert without any
markup meaning can be escaped using a single backslash.

Example:

```
\*this is not italic\*
```

→ \*this is not italic\*



## Tables

Tables are created by adding pipes as dividers between each cell, and
by adding a line of dashes (also separated by bars) beneath the header
_(this line of dashes is required)_.

- pipes do not need to be vertically aligned.
- pipes on the left and right sides of the table are sometimes optional
- three or more dashes must be used for each cell in the separator row

Example:

```
| Option | Description |
| ------ | ----------- |
| data   | path to data files to supply the data that will be passed into templates. |
| engine | engine to be used for processing templates. Handlebars is the default. |
| ext    | extension to be used for dest files. |
```

→

| Option | Description |
| ------ | ----------- |
| data   | path to data files to supply the data that will be passed into templates. |
| engine | engine to be used for processing templates. Handlebars is the default. |
| ext    | extension to be used for dest files. |


### Aligning cells

**Center text in a column**

To center the text in a column, add a colon to the middle of the dashes in the row beneath the header.

```
| Option | Description |
| :---: | :---: |
| data   | path to data files to supply the data that will be passed into templates. |
| engine | engine to be used for processing templates. Handlebars is the default. |
| ext    | extension to be used for dest files. |
```

| Option | Description |
| :--: | :---: |
| data   | path to data files to supply the data that will be passed into templates. |
| engine | engine to be used for processing templates. Handlebars is the default. |
| ext    | extension to be used for dest files. |


**Right-align the text in a column**

To right-align the text in a column, add a colon to the middle of the dashes in the row beneath the header.

```
| Option | Description |
| ------:| -----------:|
| data   | path to data files to supply the data that will be passed into templates. |
| engine | engine to be used for processing templates. Handlebars is the default. |
| ext    | extension to be used for dest files. |
```

→

| Option | Description |
| ------:| -----------:|
| data   | path to data files to supply the data that will be passed into templates. |
| engine | engine to be used for processing templates. Handlebars is the default. |
| ext    | extension to be used for dest files. |





## Additional Information

### What is CommonMark?

CommonMark is a formalisation of Markdown, "a plain text format for
writing structured documents, based on formatting conventions from
email and usenet"

Sites like GitHub and Stackoverflow have popularized the use markdown
as a plain-text alternative to traditional text editors, for writing
things like documentation and comments.

### Other Resources

- [CommonMark](https://commonmark.org/) - "A strongly defined, highly
  compatible specification of CommonMark"

|]
