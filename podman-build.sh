#!/usr/bin/env bash
# Builds the static Linux binary (musl) of a Haskell project in an Alpine
# container, with rootless podman. Generic: the package is the one described by
# the single .cabal file of the project.
#
# Usage, from the root directory of the project:
#   podman-build.sh [--update] [--clean]
#   --update   refresh the Hackage package index before building
#   --clean    remove the build directory (dist-musl) before building
#
# - Rootless podman maps the root user of the container to the current user:
#   every file written in the project belongs to that user.
# - The build uses its own directory (dist-musl), separate from the
#   dist-newstyle of the host development builds.
# - The cabal state (package index, compiled dependencies) is kept between
#   runs in $CACHE_DIR, shared by all the projects built with the same image,
#   so each dependency is compiled only once.
#
# The result is written to release/, stripped, with its SHA-256 checksum.
#
# Settings (environment variables):
#   IMAGE      container image  (default: ghc-musl 9.10.3)
#   CACHE_DIR  cabal state      (default: ~/.cache/cabal-musl/IMAGE-TAG)
#   EXE        executable       (default: the name of the package)

set -euo pipefail

IMAGE="${IMAGE:-docker.io/benz0li/ghc-musl:9.10.3-int-native}"
# One cache per image: the compiled dependencies depend on the exact compiler.
CACHE_DIR="${CACHE_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/cabal-musl/${IMAGE##*:}}"
PROJECT_FILE=cabal.project.musl
BUILD_DIR=dist-musl
OUT_DIR=release


update=0
clean=0
for arg in "$@"; do
  case "$arg" in
    --update) update=1 ;;
    --clean) clean=1 ;;
    -h | --help)
      sed -n '2,25s/^# \{0,1\}//p' "$0"
      exit 0
      ;;
    *)
      echo "unknown option: $arg (see --help)" >&2
      exit 2
      ;;
  esac
done

command -v podman >/dev/null || { echo "podman is not installed" >&2; exit 1; }
[ -f "$PROJECT_FILE" ] || { echo "$PROJECT_FILE not found in $PWD" >&2; exit 1; }

shopt -s nullglob
cabal_files=(*.cabal)
shopt -u nullglob
[ "${#cabal_files[@]}" -eq 1 ] || { echo "expected exactly one .cabal file in $PWD" >&2; exit 1; }
cabal_file="${cabal_files[0]}"

field() { awk -v key="$1:" 'tolower($1) == key { print $2; exit }' "$cabal_file"; }
package="$(field name)"
version="$(field version)"
[ -n "$package" ] && [ -n "$version" ] || { echo "name or version not found in $cabal_file" >&2; exit 1; }
EXE="${EXE:-$package}"

[ "$clean" -eq 1 ] && rm -rf "$BUILD_DIR"
mkdir -p "$CACHE_DIR" "$OUT_DIR"

echo "Building $EXE $version with $IMAGE"
echo "cabal cache: $CACHE_DIR"

# The values are passed as environment variables, so that the script run in
# the container needs no nested quoting.
podman run --rm \
  -e CABAL_DIR=/cabal \
  -e EXE="$EXE" \
  -e VERSION="$version" \
  -e PROJECT_FILE="$PROJECT_FILE" \
  -e BUILD_DIR="$BUILD_DIR" \
  -e OUT_DIR="$OUT_DIR" \
  -e UPDATE="$update" \
  -v "$CACHE_DIR:/cabal" \
  -v "$PWD:/src" \
  -w /src \
  --entrypoint sh \
  "$IMAGE" -euc '
    if [ "$UPDATE" -eq 1 ] || [ ! -d /cabal/packages ]; then
      cabal update
    fi
    set -- --project-file="$PROJECT_FILE" --builddir="$BUILD_DIR"
    cabal build "$@" "exe:$EXE"
    target="$OUT_DIR/$EXE-$VERSION-linux-$(uname -m)"
    cp "$(cabal list-bin "$@" "exe:$EXE")" "$target"
    strip "$target"
    echo "$target" > "$OUT_DIR/.last-build"
  '

target="$(cat "$OUT_DIR/.last-build")"
rm -f "$OUT_DIR/.last-build"

# Checks that the binary does not depend on any shared library.
if command -v file >/dev/null; then
  description="$(file -b "$target")"
  echo "$description"
  case "$description" in
    *"statically linked"* | *"static-pie linked"*) ;;
    *)
      echo "error: the binary is not statically linked, check $PROJECT_FILE" >&2
      exit 1
      ;;
  esac
fi

(cd "$OUT_DIR" && sha256sum "$(basename "$target")" > "$(basename "$target").sha256")

echo
echo "Done: $target ($(du -h "$target" | cut -f1))"
