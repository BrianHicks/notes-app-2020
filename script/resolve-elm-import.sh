#!/usr/bin/env sh
set -euo pipefail

ELM_JSON="${1:-}"
NAME="${2:-}"
if test -z "$NAME"; then
    echo "usage: $0 elm.json Name.Of.Module"
    exit 1
fi

RELATIVE_PATH="$(echo $NAME | sed 's|\.|/|g').elm"

# look locally

for DIR in $(jq -r '.["source-directories"] | join(" ")' "$ELM_JSON"); do
    FINAL="$DIR/$RELATIVE_PATH"
    if test -f $FINAL; then
        echo "$FINAL"
        exit 0
    fi
done

# look in elm-stuff

ELM_HOME="${ELM_HOME:-$HOME/.elm}"
ELM_VERSION="$(jq -r '.["elm-version"]' "$ELM_JSON")"

for PROJECT_VERSION_SRC in $(jq -r '.dependencies.direct | to_entries | map("\(.key)/\(.value)") | join(" ")' "$ELM_JSON"); do
    FINAL="$ELM_HOME/$ELM_VERSION/packages/$PROJECT_VERSION_SRC/src/$RELATIVE_PATH"
    if test -f $FINAL; then
        echo "$FINAL"
        exit 0
    fi
done

exit 1
