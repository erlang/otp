#!/usr/bin/env bash

## Error on undefined variables
set -u
echo $ERL_TOP >/dev/null

## Overridables - set as appropriate in the outer env
set -x
EDOC=${EDOC:-edoc}
CHUNKS=${CHUNKS-"-chunks"}
LIB_DIR=${LIB_DIR:-$ERL_TOP/lib}
APPS=${APPS:-$(ls -1 $LIB_DIR | grep -v Makefile)}
VERBOSE=${VERBOSE:-no}
set +x
## END of overridables

HEADERS=$(find $LIB_DIR -name \*.hrl -exec dirname '{}' \; | sort | uniq | \
          grep -v ct_run | grep -v SUITE_data | grep -v /test)
INCLUDES=$(for i in $HEADERS; do echo -n "-I $i "; done; echo)

for APP in $APPS; do
    echo -n "$APP: "
    if [ x"yes" = x"$VERBOSE" ]; then
        $EDOC $INCLUDES $CHUNKS -app $APP
    else
        $EDOC $INCLUDES $CHUNKS -app $APP 2>/dev/null >/dev/null \
            && echo ok \
            || echo failed
    fi
done
