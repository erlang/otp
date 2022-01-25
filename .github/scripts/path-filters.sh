#!/bin/bash

cat <<EOF
emulator: &emulator
  - 'make/**'
EOF

for file in erts/*; do
    if [ "$file" != "erts/epmd" ]; then
        if [ -d "$file" ]; then
            echo "  - '$file/**'"
        else
            echo "  - '$file'"
        fi
    fi
done

cat <<EOF
debug:
  - *emulator
epmd:
  - 'erts/epmd/**'
EOF

APPS=$(ls -d lib/*/doc | awk -F '/' '{print $2}')
for app in $APPS; do
    echo "$app:"
    echo "  - 'lib/$app/**'"
done
