#!/bin/sh

if [ -f "/buildroot/env.sh" ]; then
    . "/buildroot/env.sh"
fi

sudo mkdir -p -m0755 /var/run/sshd

sudo /usr/sbin/sshd

sudo service postgresql start

sudo -E bash -c "apt-get update && apt-get install -y linux-tools-common linux-tools-generic linux-tools-`uname -r`"

PATH=$PATH:$(ls -1d /usr/local/lib/erlang-*/bin | tr '\n' ':')

exec /bin/bash -c "$1"
