# Changelog for kiwi

## Unreleased changes

- `lang: auto/english` detects the language of a page from its content
  (French, English, Spanish), with a fallback language; `lang` can also
  be set per source in *kiwi.ini*.
- Passwords are hashed with Argon2: accounts created with an older
  version must be created again (`kiwi access … --create`).
- New `secure-cookie` option, to use when Kiwi is served through HTTPS.
- Session cookie is `HttpOnly` and `SameSite=Lax`; sessions expire
  after one year.
- Invalid pages are ignored with a warning instead of stopping Kiwi;
  Windows line endings are accepted, and files are read as UTF-8
  whatever the system locale.
