#!/bin/sh

__dir__="$(cd "$(dirname "$0")"; pwd)"

if [ ! -d "$__dir__/eldap_basic_SUITE_data/certs" ]; then
    echo "Creating certs..."
    (
        cd $__dir__ \
        && erlc make_certs.erl \
        && erl -noinput -eval 'make_certs:all("/dev/null", "eldap_basic_SUITE_data/certs").' -s init stop
    )
fi

docker run \
    --rm \
    -v "${__dir__}/eldap_basic_SUITE_data/certs:/opt/otp/openldap/certs" \
    -e LDAP_ENABLE_TLS=yes \
    -e LDAP_TLS_CERT_FILE=/opt/otp/openldap/certs/server/cert.pem \
    -e LDAP_TLS_KEY_FILE=/opt/otp/openldap/certs/server/keycert.pem \
    -e LDAP_TLS_CA_FILE=/opt/otp/openldap/certs/server/cacerts.pem  \
    -e LDAP_ROOT="dc=ericsson,dc=se" \
    -e LDAP_ADMIN_USERNAME="Manager" \
    -e LDAP_ADMIN_PASSWORD="hejsan" \
    -p 9877:1636 \
    -p 9876:1389 \
    bitnami/openldap:2.5
