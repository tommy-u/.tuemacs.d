http://spacemacs.org/
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

# Extra tools

brew install cscope / sudo apt-get install cscope 
pip install pycscope
https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Btags/cscope

brew install global --with-pygments --with-ctags
/
sudo apt-get install global
sudo apt-get install exuberant-ctags python-pygments
tar xvf global-6.5.3.tar.gz
cd global-6.5.3
./configure --with-exuberant-ctags=/usr/bin/ctags
make
sudo make install
https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Btags/gtags

# Layers
  markdown
  ivy
  auto-completion
  emacs-lisp
  git
  org
  spell-checking
  syntax-checking
  osx
  (c-c++ :variables
       c-c++-default-mode-for-headers 'c++-mode
       c-c++-enable-clang-support t)
  python
  cscope
  gtags
