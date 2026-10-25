#!/usr/bin/env bash
# Updates a kiwi binary to the latest release published on GitHub (Linux
# x86_64 only). Needs bash, curl, tar and sha256sum.
#
# Usage:
#   update-kiwi-linux.sh [--check] [KIWI_BINARY [CURRENT_VERSION]]
#   --check          only tell whether a newer release exists
#   KIWI_BINARY      binary to update (default: kiwi found in the PATH)
#   CURRENT_VERSION  its version (default: asked to the binary, kiwi --version)
#
# "kiwi update" downloads this script from the latest release and runs it
# with the path and the version of the binary: keep these arguments
# compatible, older binaries rely on them.
#
# The previous binary is kept next to the new one as KIWI_BINARY.old, not
# executable. Running kiwi servers are not restarted.

set -euo pipefail

RELEASES=https://github.com/PaulRivier/kiwi/releases

die() {
  echo "error: $*" >&2
  exit 1
}

check=0
args=()
for arg in "$@"; do
  case "$arg" in
    --check) check=1 ;;
    -h|--help) sed -n '2,16s/^# \{0,1\}//p' "$0"; exit 0 ;;
    -*) die "unknown option $arg" ;;
    *) args+=("$arg") ;;
  esac
done
[ "${#args[@]}" -le 2 ] || die "too many arguments, see --help"

[ "$(uname -s)-$(uname -m)" = Linux-x86_64 ] ||
  die "kiwi releases are for Linux x86_64 only"
for cmd in curl tar sha256sum; do
  command -v "$cmd" > /dev/null || die "$cmd is needed, please install it"
done

kiwi="${args[0]:-$(command -v kiwi || true)}"
[ -n "$kiwi" ] || die "kiwi not found in the PATH, give the path of its binary"
kiwi="$(readlink -f -- "$kiwi")"
[ -f "$kiwi" ] || die "$kiwi does not exist"
current="${args[1]:-$("$kiwi" --version 2> /dev/null | awk '{ print $NF }' || true)}"
[ -n "$current" ] || die "unable to get the version of $kiwi, give it after its path"

dir="$(dirname -- "$kiwi")"
tmp="$(mktemp -d)"
staged="$dir/.kiwi-update.$$"
trap 'rm -rf -- "$tmp" "$staged"' EXIT

# /releases/latest redirects to the page of the latest release, .../tag/TAG
latestUrl="$(curl -fsSL -o /dev/null -w '%{url_effective}' "$RELEASES/latest")" ||
  die "unable to reach $RELEASES"
tag="${latestUrl##*/}"
[[ "$latestUrl" == */tag/* && -n "$tag" ]] || die "no release found at $RELEASES"
curl -fsSL -o "$tmp/SHA256SUMS" "$RELEASES/download/$tag/SHA256SUMS" ||
  die "download of the checksums of release $tag failed"

# the version is the one of the package (the version of kiwi.cabal), the tag
# may differ (v0.3.5 for kiwi-0.3.5.0-linux-x86_64.tar.gz)
package="$(awk '{ sub(/^\*/, "", $2) }
                $2 ~ /^kiwi-[0-9.]+-linux-x86_64\.tar\.gz$/ { print $2; exit }' "$tmp/SHA256SUMS")"
[ -n "$package" ] || die "no Linux package in release $tag"
name="${package%.tar.gz}"
latest="${name#kiwi-}"
latest="${latest%-linux-x86_64}"

if [ "$latest" = "$current" ]; then
  echo "kiwi $current is up to date"
  exit 0
fi
if [ "$(printf '%s\n' "$current" "$latest" | sort -V | tail -n 1)" = "$current" ]; then
  echo "kiwi $current is newer than the latest release ($latest)"
  exit 0
fi
if [ "$check" = 1 ]; then
  echo "kiwi $latest is available (installed: $current), run kiwi update to install it"
  exit 0
fi

[ -w "$dir" ] || die "$dir is not writable, run the update again with sudo"

[ -t 0 ] || die "no terminal to confirm the update, run it from a terminal"
read -r -p "Update $kiwi from $current to $latest? [y/N] " answer
case "$answer" in
  y|Y|yes|Yes) ;;
  *) echo "Update cancelled"; exit 0 ;;
esac

echo "Downloading $package"
curl -fSL --progress-bar -o "$tmp/$package" "$RELEASES/download/$tag/$package" ||
  die "download failed"

# only the line of the package: --ignore-missing would accept a missing line
awk -v f="$package" '$2 == f || $2 == "*" f' "$tmp/SHA256SUMS" > "$tmp/SHA256SUMS.kiwi"
(cd "$tmp" && sha256sum --check --quiet SHA256SUMS.kiwi) || die "wrong checksum, update cancelled"

tar -xzf "$tmp/$package" -C "$tmp" "$name/kiwi" || die "unable to extract the binary"

# the new binary is copied next to the old one, then renamed over it: the
# rename is atomic, and running servers keep the old file open
cp -- "$tmp/$name/kiwi" "$staged"
chmod --reference="$kiwi" -- "$staged"
cp -p -- "$kiwi" "$kiwi.old"
chmod a-x -- "$kiwi.old"
mv -f -- "$staged" "$kiwi"

echo "kiwi updated to $latest, the previous binary is kept as $kiwi.old"
echo "Restart your kiwi servers to use the new version"
