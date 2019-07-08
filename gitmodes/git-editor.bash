#!/usr/bin/env bash

tmppipe=$(mktemp -u)
mkfifo "$tmppipe"

function finish {
    rm $tmppipe
}
trap finish EXIT

echo Edit File: $1
echo Pipe File: $tmppipe
cat < $tmppipe > /dev/null
