#!/bin/bash

set -xe

APPS=${1:-*}

shopt -s nullglob

for app in $(ls -d lib/${APPS}/ebin | awk -F "/" '{print $2}'); do
    echo "Generating docs for $app"
    erl -pa lib/megaco/examples/* -noinput -eval "eep48_to_markdown:convert_application(${app}), halt()"
done

if [ "$APPS" = '*' ] || [ $APPS = 'erts' ]; then
    erl -noinput -eval "eep48_to_markdown:convert_application(erts), halt()"
fi

if [ "$APPS" = '*' ] || [ $APPS = 'system' ]; then
    erl -noinput -eval "docgen_xml_to_markdown:convert_application(system), halt()"
fi
