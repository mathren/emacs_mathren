#!/bin/bash

EML_FILE="$1"
DONE_FILE="/tmp/emacs_tb_done_$$"

echo "EML: $EML_FILE" >> /tmp/tb_debug.log

rm -f "$DONE_FILE"

emacsclient --eval "(setq mr/eml-done-file \"$DONE_FILE\")"
emacsclient -c "$EML_FILE"

while [ ! -f "$DONE_FILE" ]; do
  sleep 0.3
done

echo "Done" >> /tmp/tb_debug.log
rm -f "$DONE_FILE"
