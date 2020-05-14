#!/usr/bin/env bash
# based on https://github.com/wolfadex/elm-snowpack-starter/blob/master/elm-module.sh, licensed MIT
set -euo pipefail

IN="${1:-}"
OUT="${2:-}"
FLAGS="${@:3}"

if test -z "$IN" || test -z "$OUT"; then
    echo "usage: ${0:-} src/Main.elm dist/elm.js --optimize"
    exit 1
fi

elm make "$FLAGS" --output="$OUT" "$IN"

# change the cmpiled elm file to not immediately call the compiled function
sed -i 's/(function(scope/function init(scope/g' "$OUT"
sed -i 's/}(this));/}/g' "$OUT"

# export the compiled function as the default export
echo "const scope = {};init(scope);const def = scope.Elm" >> "$OUT"
echo "export default def;" >> "$OUT"

while read -r line ; do
  # extract the name of the module
  if [[ $line =~ \$author\$project\$(.+)\$main ]]
  then
      name="${BASH_REMATCH[1]}"
      # add the module as a named export
      echo "export const ${name} = def.${name};" >> "$OUT"
  else
      echo "$line doesn't match" >&2
  fi
# find modules being exported with a 'main' function
done < <(egrep -o 'var \$author\$project\$(.+)\$main' "$OUT")
