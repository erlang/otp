#!/bin/bash

set -euo pipefail {0}

## Delete large files from runner to get more disk space
## See https://github.com/actions/runner-images/issues/2840
sudo rm -rf /usr/share/dotnet
sudo rm -rf /opt/ghc
sudo rm -rf "/usr/local/share/boost"
sudo rm -rf "$AGENT_TOOLSDIRECTORY"
docker system prune -a -f
df -h
