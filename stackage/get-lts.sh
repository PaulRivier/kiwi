#!/usr/bin/env bash
# Download the cabal.config of a Stackage LTS snapshot into the current
# folder, as lts-VERSION.config.
#
# Usage: get-lts.sh VERSION      (e.g. get-lts.sh 24.66)

set -euo pipefail

if [[ $# -ne 1 ]]; then
    echo "Usage: $(basename "$0") VERSION   (e.g. 24.66)" >&2
    exit 1
fi

version=$1
if [[ ! $version =~ ^[0-9]+\.[0-9]+$ ]]; then
    echo "Invalid LTS version: '$version' (expected MAJOR.MINOR, e.g. 24.66)" >&2
    exit 1
fi

name="lts-$version"
target="$name.config"
url="https://www.stackage.org/$name/cabal.config"

if [[ -e $target ]]; then
    echo "$target already exists, not overwritten" >&2
    exit 1
fi

tmp=$(mktemp "$target.XXXXXX")
trap 'rm -f "$tmp"' EXIT

if ! curl -fsSL -o "$tmp" "$url"; then
    echo "Download failed: $url (unknown snapshot?)" >&2
    exit 1
fi

if ! grep -q '^constraints:' "$tmp"; then
    echo "Unexpected content from $url (no 'constraints:' line)" >&2
    exit 1
fi

mv "$tmp" "$target"
trap - EXIT
echo "$target downloaded from $url"
