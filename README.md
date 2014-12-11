Indiebrain Dotfiles
===================

Installation
-------------------
* Make a local clone of this repository. I like to clone into a hidden directory in my user's home: ~/.dotfiles.
* Symlink to the .bashrc and .bash_profile files provided in the clone.

env
-------------------
Configures the shell environment by setting necessary variables. These variables are generally used later on by multiple shell configuration scripts.

config
-------------------
Configures attributes of the shell itself. This includes prompt formatting, console coloring, etc. It is also the appropriate place to include third-party configuration from the third_party directory. See the include blocks for git and rvm for example.

aliases
-------------------
Adds custom command aliases to the shell environment.

third_party
-------------------
This directory could be used to house configuration scripts for systems not directly related to the shell. For instance the default version of this dotfiles system configures git bash completion and the ruby version manager subsystems.

scripts
-------------------
This directory is automatically added to the user's PATH variable in the config script. Users can place their custom shell scripts in this directory and they will automatically be available the next time the shell loads. There is one caveat, the custom scripts must be made executable in order to to be made available at the prompt.
