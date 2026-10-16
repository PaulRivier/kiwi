# Kiwi wishlist

This is what I would like for Kiwi. Help welcome :


## Packaging

Make packages for Linux, MacOS, MSWindows

Make a nix-based build process so that people can build fast if they
already have most dependencies.

## Servant & API

Move to servant, and provide a API for queries.

Depending on the best option, we can also maintain a built-in HTML
interface for ease of use, or decide that this should be externalized
to a HTML/JS client, implemented with ELM for instance. Smart clients
using the API should be able to leverage the full power of content
indexing. In the long term, I would like to have a client for Emacs.


## Static content access control

For the moment static content is public (images, files). I would like
to move to a model where a static content access is allowed IF the
user is allowed to see any page that link to this static content. This
is doable, although it requires a bit of work. I like the idea.


## Prepare for off-memory

At the moment, Kiwi keeps everything in memory : pages, index, and
search-engine. This behaviour is reasonable for most databases on most
hardware, however if people start to have databases with more than
100MB of raw pages content and thousands of pages, it may eat too much
memory.

Off-loading page content is easy.

Off-loading meta data index is hard and non-desirable since it takes
negligible memory and should remain super-fast for user-experience.

Off-loading search-engine would require to switch to an other
solution, like Xapian, sqlite etc. This is definitely doable, and can
be kept as an alternative to in-memory search-engine.


## Overall code cleaning

I hacked Kiwi on my free time, and had to rush some features. Code
could be cleaned up and documented.

## Licensing

I have no strong opinion about licensing matters, and I am open to
suggestions.
