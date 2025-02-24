#!/bin/sh

CST="cst.vim"

VIM_RUNTIME_DIR="$HOME/.vim"
VIM_FTDETECT_DIR="$VIM_RUNTIME_DIR/ftdetect"
VIM_SYNTAX_DIR="$VIM_RUNTIME_DIR/syntax"

mkdir -p $VIM_RUNTIME_DIR || exit $?
mkdir -p $VIM_FTDETECT_DIR || exit $?
cp "ftdetect/$CST" "$VIM_FTDETECT_DIR/$CST" || exit $?
mkdir -p $VIM_SYNTAX_DIR || exit $?
cp "syntax/$CST" "$VIM_SYNTAX_DIR/$CST" || exit $?
