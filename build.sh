#!/usr/bin/env bash


set -eu
set -o pipefail

# liberated from https://stackoverflow.com/a/18443300/433393
realpath() {
  OURPWD=$PWD
  cd "$(dirname "$1")"
  LINK=$(readlink "$(basename "$1")")
  while [ "$LINK" ]; do
    cd "$(dirname "$LINK")"
    LINK=$(readlink "$(basename "$1")")
  done
  REALPATH="$PWD/$(basename "$1")"
  cd "$OURPWD"
  echo "$REALPATH"
}

TOOL_PATH=$(realpath .tool)
PAKET="$TOOL_PATH"/paket

if ! [ -e "$PAKET" ]
then
  dotnet tool install paket --tool-path "$TOOL_PATH" --version 5.207.0
fi
"$PAKET" "restore"

FAKE="$TOOL_PATH"/fake

if ! [ -e "$FAKE" ]
then
  dotnet tool install fake-cli --tool-path "$TOOL_PATH" --version 5.13.7
fi

if [ -z "$@" ] 
then
"$FAKE" build
else
"$FAKE" "$@"
fi
