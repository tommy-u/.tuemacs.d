http://spacemacs.org/

```bash
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
```

Copy .spacemacs file to ~/

# Origional Content
I like my key chorded hydras in user-config.

# Extra tools

Ispell
```bash
brew install ispell
```

Grab fonts.
```bash
#mac
brew tap caskroom/fonts
brew cask install font-source-code-pro
```

cscope: https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Btags/cscope
```bash
#mac
brew install cscope 
#linux
sudo apt-get install cscope 

pip install pycscope
```

gtags https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Btags/gtags
```bash
#mac
brew install global --with-pygments --with-ctags 

#linux
sudo apt-get install global
sudo apt-get install exuberant-ctags python-pygments
tar xvf global-6.5.3.tar.gz
cd global-6.5.3
./configure --with-exuberant-ctags=/usr/bin/ctags
make
sudo make install
```

# Layers I'm using
```
ivy
auto-completion
better-defaults
emacs-lisp
git
markdown
org
(shell :variables
       shell-default-height 30
       shell-default-position 'bottom)
spell-checking
syntax-checking
     (version-control :variables
                version-control-global-margin t
                version-control-diff-side 'right
                version-control-diff-tool 'diff-hl)
osx
     (c-c++ :variables
       c-c++-default-mode-for-headers 'c++-mode
       c-c++-enable-clang-support t)
python
cscope
gtags
```

# Problems
