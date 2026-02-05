#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
INSTALL_ROOT="${ROOT}/.tools/circom"
CIRCOM_REV="${CIRCOM_REV:-2eaaa6dface934356972b34cab64b25d382e59de}"
CIRCOM_VERSION="${CIRCOM_VERSION:-2.1.9}"
CIRCOM_BIN="${INSTALL_ROOT}/bin/circom"

if [ -x "$CIRCOM_BIN" ]; then
  if "$CIRCOM_BIN" --version 2>/dev/null | grep -q "circom compiler ${CIRCOM_VERSION}"; then
    exit 0
  fi
fi

if ! command -v cargo >/dev/null 2>&1; then
  echo "cargo is required to install circom (rev ${CIRCOM_REV})." >&2
  exit 1
fi

cargo install --git https://github.com/iden3/circom --rev "$CIRCOM_REV" --locked --force --root "$INSTALL_ROOT"
