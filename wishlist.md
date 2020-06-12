# Kiwi wishlist

This is what I would like for Kiwi. Help welcome :


## Packaging

Make packages for Linux, MacOS, MSWindows.  Bundle themes and
README.md with it. Maybe integrate README.md as a Kiwi page.

Make a nix-based build process so that people can build fast if they
already have most dependencies.

## Deployment

Kiwi needs a theme to run (theme = templates + CSS + JS ...). Users
should have easy options to link to a theme distributed with Kiwi. I
don't know how I should package and deploy for ease of use and
maintenance.

## Servant & API

Move to servant, and provide a API for queries.

Depending on the best option, we can also maintain a built-in HTML
interface for ease of use, or decide that this should be externalized
to a HTML/JS client, implemented with ELM for instance. Smart clients
using the API should be able to leverage the full power of content
indexing. In the long term, I would like to have a client for Emacs.

## Heist

For the moment Kiwi uses Mustache templates. While they are easy to
hack, they are not powerful, and this leads to added complexity on the
controller-side. Heist could be a better choice.

I would like to offer the possibility for "cascading customization",
that means that :

- Web Designers can create new themes
- Themes should be parametric enough to be customized by end-users
  (colors, menus ...)


## Improve CSS/JS

My skills in HTML/JS/CSS are really low, themes should be improved by
knowledgeable people. Maybe better wait for Heist.

## Static content access control

For the moment static content is public (images, files). I would like
to move to a model where a static content access is allowed IF the
user is allowed to see any page that link to this static content. This
is doable, although it requires a bit of work. I like the idea.

## Pandoc

Pandoc is fantastic, but Pandoc is heavy. Maybe switch to
commonmark-hs ?

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

I have no opinion about licensing matters, and I am open to
suggestions and education.
