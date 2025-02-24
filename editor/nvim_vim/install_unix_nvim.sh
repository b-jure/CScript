#!/bin/sh

CST="cst.vim"

NVIM_RUNTIME_DIR="$HOME/.config/nvim"
NVIM_FTDETECT_DIR="$NVIM_RUNTIME_DIR/after/ftdetect"
NVIM_FTPLUGIN_DIR="$NVIM_RUNTIME_DIR/after/ftplugin"
NVIM_SYNTAX_DIR="$NVIM_RUNTIME_DIR/after/syntax"

mkdir -p $NVIM_FTDETECT_DIR || exit $?
cp "ftdetect/$CST" "$NVIM_FTDETECT_DIR/$CST" || exit $?
mkdir -p $NVIM_FTPLUGIN_DIR || exit $?
cp "ftplugin/$CST" "$NVIM_FTPLUGIN_DIR/$CST" || exit $?
mkdir -p $NVIM_SYNTAX_DIR || exit $?
cp "syntax/$CST" "$NVIM_SYNTAX_DIR/$CST" || exit $?
