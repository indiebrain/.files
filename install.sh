#! /bin/bash
# Author: Aaron Kuehler
# Purpose: Install the dotfiles

DOTFILES_HOME=$HOME/.files
PLATFORM=$(uname)

function ensure_package_manager() {
    if [ $PLATFORM == "Darwin" ] && ! [ -x "$(command -v brew)" ]
    then
        echo "Installing Homebrew..."
        /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
        brew update
    elif [ $PLATFORM == "Linux" ]
    then
        sudo apt update
    fi
}

function ensure_dependencies() {
    local deps="emacs git"
    if [ $PLATFORM == "Darwin" ]
    then
        brew install $deps
    elif [ $PLATFORM == "Linux" ]
    then
        sudo apt install -y $deps
    fi
}

function ensure_latest_dotfiles() {
    if [ -e $DOTFILES_HOME ]
    then
        echo "Updating $DOTFILES_HOME"
        cd $DOTFILES_HOME
        git pull
    else
        echo "Installing dotfiles to: $DOTFILES_HOME"
        git clone git@github.com:indiebrain/.files.git $DOTFILES_HOME
        cd $DOTFILES_HOME
    fi
}

function tangle_files() {
    DIR=`pwd`
    FILES=""

    for file in `ls -1 | grep \.org | grep -v README.org`; do
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

function make_setup_executable() {
    local setup_script_path="$DOTFILES_HOME/setup.sh"

    chmod u+x $setup_script_path
}

ensure_package_manager
ensure_dependencies
ensure_latest_dotfiles
tangle_files
make_setup_executable

exit 0
