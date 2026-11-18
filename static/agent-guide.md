# Kiwi: guide for the agents

Kiwi is a wiki made of CommonMark files. You read and write its pages
through its API, with the rights of your account: you only see the pages
your account can read, and only change those it can write.

## Pages

- A page is identified by its source (a folder of the wiki) and its uid:
  `SOURCE/UID` in the answers. The uid never changes, even when the file
  moves or the title changes.
- A page is a header, a blank line, then the content in CommonMark:

      title: First aid
      summary: Gestures for wounds and burns
      tags: health > first aid, mountain
      lang: english

      # Wounds
      ...

- Header keys: `title`, `summary` (one short line, shown in the lists),
  `tags` (comma separated, `>` for sub-tags), `lang`, `archive`, `access`
  (admin right only), and the custom fields of the wiki (see the
  vocabulary). `uid` and `previous-uids` are kept by Kiwi: you never see nor
  write them.
- Link to another page of the same source with its uid:
  `[text](page:UID)` or `[text](page:UID#section-anchor)`. Link to a file of
  the source with `[name](file:UID)`. Never use raw HTML.

## Working with the wiki

- Search before writing: the information may already be there. Enrich an
  existing page rather than creating a close one.
- Read little: ask for the outline of a long page, then read only the
  section you need.
- Reuse the vocabulary of the wiki (tags, custom fields, their values):
  look at it before choosing the tags of a page.
- Change a page by replacing exact passages of its text, not by sending the
  whole page again: the rest of the page stays as it is, even if someone
  else changes it meanwhile.
- Change the metadata of many pages at once with a metadata script (its
  syntax: the metadata guide, `GET /api/metadata-guide`). Check it first,
  and show the user the pages it would change before changing the
  vocabulary of the wiki.
- Tell the user what you wrote, with the `SOURCE/UID` of the pages.
