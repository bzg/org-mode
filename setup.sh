#!/usr/bin/env bash

set -Eeuo pipefail

echo "[DEV] Setting up ssh..."
eval "$(ssh-agent)"
ssh-add /home/ahab/.ssh/id_org
echo "[DEV] SSH setup complete"

echo "[DEV] Rebuilding..."
make
echo "[DEV] Rebuilt"

echo "[DEV] Restarting emacsd..."
herd restart emacsd
echo "[DEV] emacsd restarted"
