#!/bin/bash

cat <<EOF
changes:
  - 'erts/**'
  - 'make/**'
  - 'lib/Makefile'
  - 'Makefile.in'
  - 'otp_build'
  - 'configure'
EOF

APPS=$(ls -d lib/*/doc | awk -F '/' '{print $2}')
for app in $APPS; do
    if find lib/$app/ -name '*.c' -o -name '*.h' \
            -o -name '*.cpp' -o -name '*.hpp' \
            -o -name '*.in' -o -name '*.ac' \
            -o -name 'configure' \
            | grep -v "lib/$app/test" \
            | grep -v "lib/$app/examples"\
            | grep . > /dev/null; then
        echo "  - 'lib/$app/**'"
    fi
done
