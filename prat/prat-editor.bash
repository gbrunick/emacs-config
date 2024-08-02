#!/usr/bin/env bash

LOCK_FILE=${1%/*}/.prat-editor-lock

echo LOCK_FILE=$LOCK_FILE

ls

if [ -p "$LOCK_FILE" ]; then
  echo SHOW_CURRENT_EDIT_BUFFER
  echo GIT_OUTPUT_START:
  echo Command failed because you are already editing a file for Git
  echo in Emacs.  You must complete or cancel that edit before
  echo you can start another interactive Git command.
  echo
  exit 1
fi

mkfifo $LOCK_FILE
echo Created pipe

# File to edit
echo File: $1

# Wait for Emacs to write to the pipe.
echo "Waiting for Emacs..."
cat < $LOCK_FILE

echo GIT_OUTPUT_START:
