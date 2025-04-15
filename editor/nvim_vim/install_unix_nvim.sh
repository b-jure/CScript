#!/bin/sh

CSCRIPT="cscript.vim"

NVIM_RUNTIME_DIR="$HOME/.config/nvim"
NVIM_FTDETECT_DIR="$NVIM_RUNTIME_DIR/after/ftdetect"
NVIM_FTPLUGIN_DIR="$NVIM_RUNTIME_DIR/after/ftplugin"
NVIM_SYNTAX_DIR="$NVIM_RUNTIME_DIR/after/syntax"

mkdir -p $NVIM_FTDETECT_DIR || exit $?
cp "ftdetect/$CSCRIPT" "$NVIM_FTDETECT_DIR/$CSCRIPT" || exit $?
mkdir -p $NVIM_FTPLUGIN_DIR || exit $?
cp "ftplugin/$CSCRIPT" "$NVIM_FTPLUGIN_DIR/$CSCRIPT" || exit $?
mkdir -p $NVIM_SYNTAX_DIR || exit $?
cp "syntax/$CSCRIPT" "$NVIM_SYNTAX_DIR/$CSCRIPT" || exit $?
