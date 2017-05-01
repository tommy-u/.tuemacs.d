# .tuemacs.d

There are two things going on here, 1) my custom emacs which I've been neglecting recently and 2) my spacemacs config which I've been more active on. If you have suggestions, lemme know!

## If you'd like to give my spacemacs a try, checkout the readme in my-spacemacs/

## If you'd like to give my emacs setup a try...
Pull this repo down into ~/
Install ispell, gnu global, w3m (see below for Mac).
Write the following line into you .emacs file: 
```lisp
(load-file "~/.tuemacs.d/init.el")
```

A few lines will be automatically generated in .emacs.
A directory .emacs.d/elpa will be generated holding package information. 

Heads up:
Magit requires emacs 23.4, https://github.com/magit/magit/issues/2044, so you will get errors on the still popular 23.3 version.

Company-jedi:
As far as I know, it is necessary to run:
```
M-x jedi:install-server 
```
by hand to get that running.
Requires virtualenv to install server (below).

Install ispell, gnu global, w3m:
```bash
brew install ispell globak w3m
```
Install virtualenv:
```bash
pip3 install virtualenv
```
