#!/bin/sh

CSCRIPT="cscript.vim"

VIM_RUNTIME_DIR="$HOME/.vim"
VIM_FTDETECT_DIR="$VIM_RUNTIME_DIR/ftdetect"
VIM_SYNTAX_DIR="$VIM_RUNTIME_DIR/syntax"

mkdir -p $VIM_RUNTIME_DIR || exit $?
mkdir -p $VIM_FTDETECT_DIR || exit $?
cp "ftdetect/$CSCRIPT" "$VIM_FTDETECT_DIR/$CSCRIPT" || exit $?
mkdir -p $VIM_SYNTAX_DIR || exit $?
cp "syntax/$CSCRIPT" "$VIM_SYNTAX_DIR/$CSCRIPT" || exit $?
