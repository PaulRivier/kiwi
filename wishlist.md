# Kiwi wishlist

This is what I would like for Kiwi. Help welcome :


## Packaging

Make packages for Linux, MacOS, MSWindows.  Bundle themes and
README.md with it. Maybe integrate README.md as a Kiwi page.

Make a nix-based build process.

## Deployment

Kiwi needs a theme to run (theme = templates + CSS + JS ...). Users
should have easy options to link to a theme distributed with Kiwi.

## Servant

Move to servant, provide a API for queries, and maintain an HTML
access. HTML access is good for people that want something easy to
use. API is good to leverage the full power of content indexing, and
this could be used by various clients : HTML/JS, Gnome, Emacs, Vim ...

## Heist

For the moment Kiwi uses Mustache templates. While they are easy to
hack, they are not really powerful, and this can lead to added
complexity on the controller-side. Heist could be a better choice.

## Improve CSS/JS

My skills in HTML/JS/CSS are really low, themes should be improved by
knowledgeable people. Maybe better wait for Heist.

## Static content access control

For the moment static content is public (images, files). I would like
to move to a model where a static content is allowed if the user is
allowed to see a page that link to this content.

## Pandoc

Pandoc is fantastic, but Pandoc is heavy. Maybe switch to
commonmark-hs.

## Overall code cleaning

I hacked Kiwi on my free time, and had to rush some features. Code
could be cleaned up and documented.
