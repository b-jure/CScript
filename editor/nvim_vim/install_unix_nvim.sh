#!/bin/sh

TOKUDAE="tokudae.vim"

NVIM_RUNTIME_DIR="$HOME/.config/nvim"
NVIM_FTDETECT_DIR="$NVIM_RUNTIME_DIR/after/ftdetect"
NVIM_FTPLUGIN_DIR="$NVIM_RUNTIME_DIR/after/ftplugin"
NVIM_SYNTAX_DIR="$NVIM_RUNTIME_DIR/after/syntax"

mkdir -p $NVIM_FTDETECT_DIR || exit $?
cp "ftdetect/$TOKUDAE" "$NVIM_FTDETECT_DIR/$TOKUDAE" || exit $?
mkdir -p $NVIM_FTPLUGIN_DIR || exit $?
cp "ftplugin/$TOKUDAE" "$NVIM_FTPLUGIN_DIR/$TOKUDAE" || exit $?
mkdir -p $NVIM_SYNTAX_DIR || exit $?
cp "syntax/$TOKUDAE" "$NVIM_SYNTAX_DIR/$TOKUDAE" || exit $?
