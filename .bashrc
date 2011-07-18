#!/usr/bin/env bash

# Setup the dotfile home dynamically
if [ -s ~/.bashrc ];
    then 
    FILE_PATH=`readlink ~/.bashrc`
    BASE_CONFIG_PATH=`dirname $FILE_PATH`
fi

# Read in the environment configuration
ENV_FILE=$BASE_CONFIG_PATH/env
if [ -f $ENV_FILE ];
    then source $ENV_FILE
fi

# Read in the shell configuration
CONFIG_FILE=$BASE_CONFIG_PATH/config
if [ -f $CONFIG_FILE ];
    then source $CONFIG_FILE
fi

# Read in the alias configuration
ALIAS_FILE=$BASE_CONFIG_PATH/aliases
if [ -f $ALIAS_FILE ];
    then source $ALIAS_FILE
fi