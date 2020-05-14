#!/usr/bin/env bash
set -eou pipefail

ELM_FILES="$(find src -name '*.elm')"

cat <<EOF
elmFlags=--debug

rule elm
  command = elm make \$elmFlags --output=\$out \$in

rule minify
  command = uglifyjs \$in > \$out

rule copy
  command = cp \$in \$out

build dist/index.html: copy src/index.html
build dist/script/custom-elements-builtin-bootstrap.js: minify src/custom-elements-builtin-bootstrap.js
build dist/script/custom-elements-builtin.js: minify vendor/custom-elements-builtin.js
build dist/script/elm.js: elm src/Main.elm | elm.json $(echo "$ELM_FILES" | tr '\n' ' ')
build dist/script/index.js: copy src/index.js
build dist/script/pouchdb.js: minify vendor/pouchdb-7.1.1.js
EOF
