# Changelog for kiwi

TODO

## Unreleased

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
