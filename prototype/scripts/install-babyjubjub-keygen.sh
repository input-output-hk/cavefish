#!/usr/bin/env bash
set -euo pipefail

# Selects (or builds) a `babyjubjub-keygen` binary that runs on this host and
# exposes it as `.tools/babyjubjub-keygen/bin/babyjubjub-keygen` (the nix shell
# adds that directory to PATH; outside nix, add it yourself).
#
# Order of preference:
#   1. an already installed, working binary in .tools/babyjubjub-keygen/bin
#   2. a prebuilt binary matching this platform in packages/wbps/setup/bin
#      (babyjubjub-keygen-<arch>-<os>, e.g. x86_64-linux, aarch64-linux)
#   3. a native build via ../zk-wbps/tooling/gen_babyjubjub_keys.sh
#      (requires cargo and network access)

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PREBUILT_DIR="${ROOT}/packages/wbps/setup/bin"
INSTALL_DIR="${ROOT}/.tools/babyjubjub-keygen/bin"
TARGET="${INSTALL_DIR}/babyjubjub-keygen"
GEN_SCRIPT="${ROOT}/../zk-wbps/tooling/gen_babyjubjub_keys.sh"

works() {
  # A working keygen prints a JSON object with an "ek" field and exits 0.
  [ -x "$1" ] && "$1" 2>/dev/null | grep -q '"ek"'
}

if works "$TARGET"; then
  exit 0
fi

mkdir -p "$INSTALL_DIR"
rm -f "$TARGET"

os="$(uname -s | tr '[:upper:]' '[:lower:]')" # linux, darwin, ...
arch="$(uname -m)"                             # x86_64, aarch64, arm64, ...
case "$arch" in
  arm64) arch="aarch64" ;;
  amd64) arch="x86_64" ;;
esac

candidate="${PREBUILT_DIR}/babyjubjub-keygen-${arch}-${os}"
if works "$candidate"; then
  ln -sf "$candidate" "$TARGET"
  echo "babyjubjub-keygen: using prebuilt $(basename "$candidate")"
  exit 0
fi

if ! command -v cargo >/dev/null 2>&1; then
  echo "babyjubjub-keygen: no working prebuilt binary for ${arch}-${os} and cargo is not available." >&2
  echo "Install cargo, then run: DEST=${TARGET} bash ${GEN_SCRIPT}" >&2
  exit 1
fi

echo "babyjubjub-keygen: no prebuilt binary for ${arch}-${os}; building natively (needs network access)..."
DEST="$TARGET" bash "$GEN_SCRIPT" >/dev/null
if ! works "$TARGET"; then
  echo "babyjubjub-keygen: native build failed (see ${GEN_SCRIPT})" >&2
  exit 1
fi
echo "babyjubjub-keygen: built at ${TARGET}"
