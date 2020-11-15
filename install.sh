#! /bin/bash
# Author: Aaron Kuehler
# Purpose: Install the dotfiles

DOTFILES_HOME="$HOME/.files"
PLATFORM="$(uname)"

function ensure_package_manager() {
    if [ "$PLATFORM" == "Darwin" ] && ! [ -x "$(command -v brew)" ]
    then
        echo "Installing Homebrew..."
        /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
        brew update
    elif [ "$PLATFORM" == "Linux" ]
    then
        sudo apt update
    fi
}

function ensure_macos_package() {
    local package
    package="$1"

    if ! command -v "$package" &> /dev/null
        then
            brew install "$package"
    fi
}

function ensure_macos_package() {
    local package
    package="$1"

    if ! command -v "$package" &> /dev/null
    then
        sudo apt install -y "$package"
    fi
}



function ensure_dependencies() {
    if [ "$PLATFORM" == "Darwin" ]
    then
        ensure_macos_package "emacs"
        ensure_macos_package "git"
    elif [ "$PLATFORM" == "Linux" ]
    then
        ensure_apt_package "emacs"
        ensure_apt_package "git"
    fi
}

function ensure_dotfiles() {
    if [ ! -e "$DOTFILES_HOME" ]
    then
        echo "Installing dotfiles to: $DOTFILES_HOME"
        git clone git@github.com:indiebrain/.files.git "$DOTFILES_HOME"
        cd "$DOTFILES_HOME" || (echo "Failed to change directory to '$DOTFILES_HOME'" &&  exit 1)
    fi
}

function tangle_files() {
    cd "$DOTFILES_HOME" || (echo "Failed to change directory to '$DOTFILES_HOME'" &&  exit 1)
    FILES=""

    for file in $(ls -1 ./*.org)
    do
        FILES="$FILES \"$file\""
    done

    echo -e "Installing: \n$FILES"
    emacs -Q --batch \
          --eval \
          "(progn
            (require 'org)(require 'ob)(require 'ob-tangle)
             (mapc (lambda (file)
                     (find-file (expand-file-name file \"$DOTFILES_HOME\"))
                     (org-babel-tangle)
                     (kill-buffer)) '($FILES)))"
}

function make_setup_executable() {
    local setup_script_path="$DOTFILES_HOME/setup.sh"
    setup_script_path="$DOTFILES_HOME/setup.sh"

    chmod u+x "$setup_script_path"
}

ensure_package_manager
ensure_dependencies
ensure_dotfiles
tangle_files
make_setup_executable

exit 0
