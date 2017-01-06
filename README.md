# .tuemacs.d

If you'd like to give this a try, pull this directory down into ~. Install ispell, gnu global, w3m (see below for Mac). Write the following line into you .emacs file: 
(load-file "~/.tuemacs.d/init.el")

A few lines will be automatically generated in .emacs. A directory .emacs.d/elpa will be generated holding package information. 

Notes:
Magit requires emacs 23.4, https://github.com/magit/magit/issues/2044, so will not run on the still popular 23.3 version.

need to install ispell, gnu global, w3m

brew install ispell
brew install global
brew install w3m
