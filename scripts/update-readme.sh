#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

new_version=$(git describe --tags --abbrev=0) # latest tag
new_version=${new_version#v} # strip v prefix

# update ocurrences of % "x.y.z"
perl -pi -e "s/% \"[0-9\.]+\"/% \"$new_version\"/g" README.md
