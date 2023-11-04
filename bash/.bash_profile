#
# Name:
#   ~/.bash_profile
#
# Purpose:
#
#  Part of my dotfiles https://github.com/indiebrain/.files
#
#  Configure Login Shells. See the GNU Bash manuals for information on
#  startup files:
#  https://www.gnu.org/software/bash/manual/html_node/Bash-Startup-Files.html.



## If a Bourne Login Shell configuration exists, load it
[ -r ~/.profile ] && . ~/.profile || true

# If Bash Login Shell configuration exists, load it
[ -r ~/.bash_login ] && . ~/.bash_login || true

## Is this an interactive shell?
if [ -n "$PS1" ]
then
    # If interactive shell configuration exists, load it
    [ -r ~/.bashrc ] && . ~/.bashrc || true
fi

export ARCHFLAGS="-arch arm64"
export PATH="/opt/homebrew/opt/openssl@1.1/bin:$PATH"
export LIBRARY_PATH="/opt/homebrew/opt/openssl@1.1/lib:$LIBRARY_PATH"
export PATH="/opt/homebrew/opt/mysql-client/bin:$PATH"
export PATH="/opt/homebrew/opt/postgresql@11/bin:$PATH"
