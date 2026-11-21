# Kiwi: a page as a local file

Only if you have a shell and the environment variable KIWI_API_TOKEN holds
your API token. Never write the token itself in a command. curl 7.87 or
later.

1. Download the text of the page (header and content) to a file; the
   command prints the ETag of the page, keep it:

       curl KIWI_URL/api/page/SOURCE/UID -sS -o FILE -w '%header{etag}\n' -H "Authorization: Bearer $KIWI_API_TOKEN"

2. Change the file with your own tools. Read a long file only in the
   parts you need. Keep the header, then a blank line, then the content;
   do not add a uid.
3. Send the file back whole:

       curl KIWI_URL/api/page/SOURCE/UID -sS -X PUT --data-binary @FILE -H "Authorization: Bearer $KIWI_API_TOKEN" -H "If-Match: ETAG" --url-query "message=MESSAGE"

   Use `--data-binary`, never `-d`, which removes the line ends. A 412
   answer means that the page changed meanwhile: download it again and
   redo your changes. The other errors say what is wrong.
4. Several pages: the same for each one, one file per page.
