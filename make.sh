#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo $SCRIPT_DIR

ln -fnsv ${SCRIPT_DIR}/.bashrc "$HOME"
ln -fnsv ${SCRIPT_DIR}/.emacs "$HOME"
ln -fnsv ${SCRIPT_DIR}/.gitconfig "$HOME"
mkdir -p $HOME/.config/git && ln -fnsv ${SCRIPT_DIR}/.gitignore_global  "$HOME/.config/git/ignore"

