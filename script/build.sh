#!/usr/bin/env bash
set -eou pipefail

ELM_FILES="$(find src -name '*.elm')"

cat > build.ninja <<EOF
elmFlags=--debug

rule elm
  command = elm make \$elmFlags --output=\$out \$in

build dist/elm.js: elm src/Main.elm | elm.json $(echo "$ELM_FILES" | tr '\n' ' ')
EOF

ninja "$@"
