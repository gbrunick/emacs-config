#!/usr/bin/env bash

tmppipe=$(mktemp -u)
mkfifo "$tmppipe"

function finish {
    rm $tmppipe
    echo Deleted $tmppipe
}
trap finish EXIT

echo e7010240-6f57-4d86-84f9-62fb8958b7a6:edit-file:$1
echo e7010240-6f57-4d86-84f9-62fb8958b7a6:pipe-file:$tmppipe

# Wait for Emacs to write to the pipe.
cat < $tmppipe > /dev/null
