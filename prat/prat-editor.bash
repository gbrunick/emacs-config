#!/usr/bin/env bash

mkfifo "$PRAT_EDITOR_PIPE"
echo Created pipe: "$PRAT_EDITOR_PIPE"

# File to edit
echo File: $1

# Wait for Emacs to write to the pipe.
echo "Waiting for Emacs..."
cat < $PRAT_EDITOR_PIPE

# Clean up
rm $PRAT_EDITOR_PIPE
echo Deleted pipe: $PRAT_EDITOR_PIPE
echo Command output:
