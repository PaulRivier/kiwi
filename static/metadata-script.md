# Metadata scripts

A metadata script changes the headers of many pages at once: renaming a
tag, merging two values of a field, setting the summary of a few pages...
It runs on the pages that the account can change, archives included.

- One directive or command by line. Blank lines and lines starting with `#`
  are ignored (a `#` after a command is not a comment).
- All the commands apply, in their order, or none: the first error stops
  everything, and nothing is written.
- The changed files are written in one git commit, whose message holds the
  script: `git revert` undoes it.
- Check a script first: it then writes nothing, and lists the pages that
  each command would change. The check runs on the pages that the account
  can read: those it can not change are marked `(read only)`, and an
  account that can change them (an admin) applies the script.

## Choosing the pages: `pages`

A `pages` line chooses the pages of the commands that follow it, until the
next `pages` line. A command before any `pages` line is an error.

- `pages cuisine/*`: the pages of the source `cuisine`;
- `pages cuisine/*,plantes/*`: the pages of two sources;
- `pages cuisine/3VBQJ37HE5`: one page, by its source and uid;
- `pages cuisine/3VBQJ37HE5,plantes/CTAMPPRG36`: two pages;
- `pages *`: every page.

- The items are separated by commas; `SOURCE/*` and `SOURCE/UID` can be
  mixed.
- A source whose name has spaces is between double quotes:
  `pages "My notes"/*`.
- With `*` or `SOURCE/*`, the pages that the account can not change are left
  out. A page named by its uid that the account can not change is an error.
  (When checked: the pages that the account can read, see above.)
- After commands on a few pages, write `pages SOURCE/*` again before a
  command meant for the whole source: forgetting it is the easiest mistake
  to make, and the number of pages of each command, in the answer, shows
  it.

## Commands

    meta FIELD VERB ARGUMENTS

- `FIELD`: `tags`, `title`, `summary`, `lang`, `archive`, `access` (admin
  right on the page), or a custom field. `uid` and `previous-uids` can not
  be changed.
- An argument with spaces, commas or `>` is between double quotes, with
  `\"` and `\\` inside: `"cuisine > matériel"`.
- A value is written as in a header: `"sport > climbing, health"` is two
  tags. A value that a header could not hold (an invalid date, an unknown
  lang) is an error.

The verbs:

- `meta FIELD move OLD NEW`: the value OLD becomes NEW. For `tags`, OLD and
  its sub-tags become NEW and its sub-tags; a page that has both keeps one.
- `meta FIELD delete VALUE`: the value is removed. For `tags`, the tag
  **and its sub-tags**.
- `meta FIELD rename-to NEW`: the field becomes the field NEW (custom
  fields only; refused on a page that already has NEW).
- `meta FIELD set VALUE`: the field gets this value, replacing the former
  one.
- `meta FIELD add VALUE`: values added to a list (`tags`, custom fields).
- `meta FIELD remove VALUE`: values removed from a list, **exact values
  only**: `meta tags remove sport` keeps `sport > climbing`. To remove a tag
  with its sub-tags, use `delete`.
- `meta FIELD unset`: the field is removed.

`title`, `summary`, `lang` and `archive` have one value: `set` and `unset`
only.

## Errors

- A command on `*` or `SOURCE/*` pages that changes no page is an error: a
  typo, most likely. For a tag, the error gives the close tags.
- On pages named by their uid, a page that already has the wanted value
  counts for 0, without error.
- The errors give the line, and for a syntax error the column and what was
  expected.

## Examples

    # one root tag for the tools
    pages cuisine/*,maison/*
    meta tags move "cuisine > matériel" "matériel > cuisine"
    meta tags move "maison > matériel" "matériel > maison"

    # one spelling for the ingredients
    pages cuisine/*
    meta ingrédients move bananes banane

    # a few pages
    pages cuisine/3VBQJ37HE5,cuisine/CTAMPPRG36
    meta tags add "sport > matériel"
    meta summary set "Hiking gear"

    # a field that is no longer used
    pages *
    meta rating unset
