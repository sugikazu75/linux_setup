#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo $SCRIPT_DIR

ln -fnsv ${SCRIPT_DIR}/.bashrc "$HOME"
ln -fnsv ${SCRIPT_DIR}/.emacs "$HOME"
ln -fnsv ${SCRIPT_DIR}/.gitconfig "$HOME"
mkdir -p $HOME/.config/git && ln -fnsv ${SCRIPT_DIR}/.gitignore_global  "$HOME/.config/git/ignore"
ln -fnsv ${SCRIPT_DIR}/.tmux.conf "$HOME"

if !(type wget > /dev/null 2>&1); then
    sudo apt install wget
fi
wget https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh -O ${SCRIPT_DIR}/.git-prompt.sh
ln -fnsv ${SCRIPT_DIR}/.git-prompt.sh "$HOME"

if [ $WSLENV ]; then
    sudo ln -fnsv ${SCRIPT_DIR}/wsl.conf /etc/wsl.conf
    sudo ln -fnsv ${SCRIPT_DIR}/wsl_resolv.conf /etc/resolv.conf
fi
