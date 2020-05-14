#!/usr/bin/env bash
set -euo pipefail

FILE="${1:-}"
EXPORT="${2:-}"
if test -z "${FILE}" || test -z "${EXPORT}"; then
    echo "usage: ${0:-} file thingToExport"
fi

sed '1 i\var exports = {};\nvar module = {};' "$FILE" | sed "\$ a\export const ${EXPORT} = module.exports;"
