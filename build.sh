#!/usr/bin/env bash
# usage   build.sh CABAL_ACTION TARGET [CABAL_OPTIONS...]
# ex.     build.sh build devel
# ex.     build.sh run devel kiwi -- serve kiwi.ini
#         (cabal run needs the executable name before the -- )
#
# The project file is cabal.project.TARGET, the build directory dist/TARGET.
# A build of a target other than devel also writes the stripped binary to
# release/.

set -euo pipefail

[ $# -ge 2 ] || { echo "usage: $0 CABAL_ACTION TARGET [CABAL_OPTIONS...]" >&2; exit 1; }
ACTION=$1
TARGET=$2
shift 2

PROJECT_FILE="cabal.project.$TARGET"
BUILD_DIR="dist/$TARGET"
OUT_DIR=release


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


cabal "$ACTION" --project-file="$PROJECT_FILE" --builddir="$BUILD_DIR" "$@"

# only an optimized build produces a release binary
[ "$ACTION" = build ] && [ "$TARGET" != devel ] || exit 0

mkdir -p "$OUT_DIR"
bin_path="$OUT_DIR/$EXE-$version-$TARGET-linux-$(uname -m)"
cp "$(cabal list-bin --project-file="$PROJECT_FILE" --builddir="$BUILD_DIR" "exe:$EXE")" "$bin_path"
strip "$bin_path"
echo "Done: $bin_path"
