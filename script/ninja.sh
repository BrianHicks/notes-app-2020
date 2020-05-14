#!/usr/bin/env bash
set -eou pipefail

ELM_FILES="$(find src -name '*.elm')"

cat <<EOF
elmFlags=--debug

rule elm
  command = ./script/elm-make-module.sh \$in \$out \$elmFlags

rule es6ify
  command = ./script/es6ify.sh \$in \$export > \$out

rule copy
  command = cp \$in \$out

build dist/index.html: copy src/index.html
build dist/script/custom-elements-builtin-bootstrap.js: copy src/custom-elements-builtin-bootstrap.js
build dist/script/custom-elements-builtin.js: copy vendor/custom-elements-builtin.js
build dist/script/elm.js: elm src/Main.elm | elm.json $(echo "$ELM_FILES" | tr '\n' ' ')
build dist/script/index.js: copy src/index.js
build dist/script/pouchdb.js: es6ify vendor/pouchdb-7.1.1.js
  export = PouchDB
EOF
