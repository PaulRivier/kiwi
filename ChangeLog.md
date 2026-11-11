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
