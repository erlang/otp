#!/bin/bash

cat <<EOF
emulator:
  - ['erts/**','!erts/epmd/**','!erts/test/**']
debug:
  - ['erts/**','!erts/epmd/**','!erts/test/**']
epmd:
  - 'erts/epmd/**'
EOF

APPS=$(ls -d lib/*/doc | awk -F '/' '{print $2}')
for app in $APPS; do
    echo "$app:"
    echo "  - 'lib/$app/**'"
done
