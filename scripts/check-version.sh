#!/usr/bin/env bash
set -euo pipefail

RELEASE="${1}"
DOWNLOAD="${2}"

CABAL_SCAFFOLD="${DOWNLOAD}/bins-Linux/cabal-scaffold"
chmod +x "${CABAL_SCAFFOLD}"

VERSION=$("${CABAL_SCAFFOLD}" --numeric-version)

if [[ "${VERSION}" = "${RELEASE}" ]]; then
    echo "Release version ok: ${VERSION}"
else
    echo "Release version mismatched!" >&2
    echo "   expected: ${RELEASE}" >&2
    echo "  cabal ver: ${VERSION}" >&2
    exit 1
fi
