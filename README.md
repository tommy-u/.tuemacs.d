# .tuemacs.d

If you'd like to give this a try
Pull this directory down into ~.
Install ispell, gnu global, w3m (see below for Mac).
Write the following line into you .emacs file: (load-file "~/.tuemacs.d/init.el")

A few lines will be automatically generated in .emacs.
A directory .emacs.d/elpa will be generated holding package information. 

Heads up:
Magit requires emacs 23.4, https://github.com/magit/magit/issues/2044, so you will get errors on the still popular 23.3 version.

Company-jedi:
As far as I know, it is necessary to run M-x jedi:install-server by hand to get that running.
Requires virtualenv to install server.


Install ispell, gnu global, w3m:
brew install ispell globak w3m

Install virtualenv:
pip3 install virtualenv
