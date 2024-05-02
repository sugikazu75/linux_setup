#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo $SCRIPT_DIR

ln -fnsv ${SCRIPT_DIR}/.bashrc "$HOME"
ln -fnsv ${SCRIPT_DIR}/.emacs "$HOME"
ln -fnsv ${SCRIPT_DIR}/.gitconfig "$HOME"
ln -fnsv ${SCRIPT_DIR}/.gitignore_global  "$HOME/.config/git/ignore"

