#!/usr/bin/env bash

set -Eeuo pipefail

make autoloads
emacs -q --eval="(add-to-list 'load-path \"/home/ahab/Projects/org-mode/lisp\")"
