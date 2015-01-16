#! /bin/bash
# Author: Aaron Kuehler
# Purpose: Install the bash configurations.
#          NOTE: https://twitter.com/jimweirich/status/29662011323326464
SCRIPT_DIR=`dirname "${BASH_SOURCE[0]}"`
cd $SCRIPT_DIR

DOTFILES_HOME=`pwd`

for file in $DOTFILES_HOME/{.bash,.git}?*
do
    ln -s $file ~/
done
