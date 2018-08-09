This project exists to give me an easy way to keep my emacs configuration
files in sync.

Everything in this project is copywrite Gerard Brunick 2013 to the extent
that it can be.

The .emacs file should: (load "path\to\gpb-init.el")

You should also install the following packages:
  auto-complete
  auctex
  bm
  browse-kill-ring
  company
  dired-single
  jedi
  xml-rpc
  helm
  elpy
  dired-single



Current Bugs:

gpb-logging - the broken indentation.

gpb-python - problems with locations of errors in compilation-shell-minor
             mode in ipython buffers.

gpb-settings - gpb-settings:make-settings-buffer should be a macro
               gpb-settings:defun.

post `gpb-jedi:xml-rpc-method-call-async' to bazaar cite for xml-rpc.
