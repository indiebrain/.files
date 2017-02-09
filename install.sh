#! /bin/bash
# Author: Aaron Kuehler
# Purpose: Install the dotfiles

DOTFILES_HOME=$HOME/.files

function clone_or_update_repo() {
    if [ -e $DOTFILES_HOME ]; then
        echo "Updating $DOTFILES_HOME"
        pushd $DOTFILES_HOME
        git pull
    else
        echo "Installing dotfiles to: $DOTFILES_HOME"
        git clone https://github.com/indibrain/.files $DOTFILES_HOME
        pushd $DOTFILES_HOME
    fi
}


function tangle_files() {
    DIR=`pwd`
    FILES=""

    for file in `ls -1 | grep \.org`; do
        FILES="$FILES \"$file\""
    done

    echo -e "Installing: \n$FILES"
    emacs -Q --batch \
          --eval \
          "(progn
            (require 'org)(require 'ob)(require 'ob-tangle)
             (mapc (lambda (file)
                     (find-file (expand-file-name file \"$DIR\"))
                     (org-babel-tangle)
                     (kill-buffer)) '($FILES)))"
}

clone_or_update_repo
tangle_files

exit 0
