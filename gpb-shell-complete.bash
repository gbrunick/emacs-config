# We need to fake a terminal to get readline behaviour from Bash.
export TERM=vt100
exec bash

# Suppress the query that asks if you really want to list
# all this items.
bind "set completion-query-items -1"

# Suppress the \C-g character in the output
bind "set bell-style none"

# Don't use a pager.
bind "set page-completions Off"

# Bind tab to possible-completions so we always get a list
# from readline after the first TAB we send.
bind TAB:possible-completions

# Use a single column, so its one choice per output line.
bind "set completion-display-width 0"
