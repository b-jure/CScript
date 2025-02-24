" CScript syntax file
" Language:     CScript 1.0
" Maintainer:   Jure Bagić <jurebagic99@gmail.com>
" Last Change:  2025 Feb 22 


" quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

" case sensitive
syn case match


"-Keywords--------{
syn keyword     cscriptStatement        break return continue local
syn keyword     cscriptConditional      if else switch
syn keyword     cscriptOperator         and or
syn keyword     cscriptLabel            case default
syn keyword     cscriptRepeat           while for foreach loop
syn keyword     cscriptConstant         true false nil
"-----------------}


"-Foreach---------{
syn region      cscriptForEach          transparent matchgroup=cscriptRepeat start=/\<foreach\>/ end=/\<in\>/me=e-2 contains=TOP skipwhite skipempty
syn keyword     cscriptForEach          contained containedin=cscriptForEach in
"-----------------}


"-Classes---------{
syn region      cscriptClass            transparent matchgroup=cscriptStatement start=/\<class\>/ end=/{/me=e-1 contains=TOP skipwhite skipempty
syn keyword     cscriptClass            contained containedin=cscriptClass inherits
"-----------------}


"-Operators-------{
syn match       cscriptSymbolOperator   /[<>=~^*&|/%+-]\|\.{2,3}/
"-----------------}


"-Functions-------{
syn region      cscriptFunctionBlock    transparent matchgroup=cscriptFunction start=/\<fn\>/ end=/}/ contains=TOP
syn keyword     cscriptSuper            contained containedin=cscriptFunctionBlock super

syn keyword     cscriptMetaMethod       __getidx __setidx
syn keyword     cscriptMetaMethod       __gc __close __call __init __concat
syn keyword     cscriptMetaMethod       __mod __pow __add __sub __mul __div
syn keyword     cscriptMetaMethod       __shl __shr __band __bor __bxor
syn keyword     cscriptMetaMethod       __unm __bnot
syn keyword     cscriptMetaMethod       __eq __lt __le

" basic library
syn keyword     cscriptFunc             error assert gc load loadfile runfile
syn keyword     cscriptFunc             getmetamethod next pairs ipairs pcall
syn keyword     cscriptFunc             xpcall print warn len rawequal rawget
syn keyword     cscriptFunc             rawset getargs tonumber tostring typeof
syn keyword     cscriptFunc             getclass __G __VERSION

" package library
syn keyword     cscriptFunc             import
syn match       cscriptFunc             /\<package\.loadlib\>/
syn match       cscriptFunc             /\<package\.searchpath\>/
syn match       cscriptFunc             /\<package\.preload\>/
syn match       cscriptFunc             /\<package\.cpath\>/
syn match       cscriptFunc             /\<package\.path\>/
syn match       cscriptFunc             /\<package\.searchers\>/
syn match       cscriptFunc             /\<package\.loaded\>/

" string library
syn match       cscriptFunc             /\<string\.split\>/
syn match       cscriptFunc             /\<string\.rsplit\>/
syn match       cscriptFunc             /\<string\.startswith\>/
syn match       cscriptFunc             /\<string\.reverse\>/
syn match       cscriptFunc             /\<string\.repeat\>/
syn match       cscriptFunc             /\<string\.join\>/
syn match       cscriptFunc             /\<string\.format\>/
syn match       cscriptFunc             /\<string\.toupper\>/
syn match       cscriptFunc             /\<string\.tolower\>/
syn match       cscriptFunc             /\<string\.count\>/
syn match       cscriptFunc             /\<string\.find\>/
syn match       cscriptFunc             /\<string\.rfind\>/
syn match       cscriptFunc             /\<string\.replace\>/
syn match       cscriptFunc             /\<string\.substr\>/
syn match       cscriptFunc             /\<string\.swapcase\>/
syn match       cscriptFunc             /\<string\.swapupper\>/
syn match       cscriptFunc             /\<string\.swaplower\>/
"-----------------}


"-Comments--------{
syn keyword     cscriptTodo             contained TODO FIXME XXX
syn cluster     cscriptCommentGroup     contains=cscriptTodo

" single line
syn region      cscriptComment          matchgroup=cscriptCommentStart start=/#/ skip=/\\$/ end=/$/ keepend contains=@cscriptCommentGroup
syn region      cscriptComment          matchgroup=cscriptCommentStart start=/\/\// skip=/\\$/ end=/$/ keepend contains=@cscriptCommentGroup

" multi-line
if exists("c_no_comment_fold")
    syn region      cscriptComment      matchgroup=cscriptCommentStart start=/\/\*/ end=/\*\// contains=@cscriptCommentGroup,cscriptCommentStartError extend
else
    syn region      cscriptComment      matchgroup=cscriptCommentStart start=/\/\*/ end=/\*\// contains=@cscriptCommentGroup,cscriptCommentStartError fold extend
endif

" errors
syn match	cscriptCommentError         display /\*\//
syn match	cscriptCommentStartError    display /\/\*/me=e-1 contained
syn match	cscriptWrongComTail	    display /\*\//
"-----------------}


"-Strings---------{
" highlight special characters (those which have a backslash) differently
syn match	cscriptSpecial	        display contained /\\[\\abtnvfre"[\]]/
" highlight decimal escape sequence \ddd
syn match	cscriptSpecial	        display contained /\\[[:digit:]]\{,3}/
" highlight hexadecimal escape sequence \xx
syn match	cscriptSpecial	        display contained /\\x[[:xdigit:]]\{2}/
" highlight unicode escape sequence \u{xx} or \u[xx]
syn match	cscriptSpecial	        display contained /\\u\%({\|\[\)[[:xdigit:]]\+\%(}\|\]\)/

syn region      cscriptString           start=/"/   skip=/\\"/  end=/"/ contains=cscriptSpecial,@Spell
"-----------------}


"-Characters------{
syn match       cscriptCharacter        /'[^']'/ contains=cscriptSpecial
"-----------------}


"-Blocks----------{
if exists("c_curly_error")
    syn match       cscriptCurlyError       /}/
    syn region      cscriptBlock            start=/{/   end=/}/     contains=TOP,cscriptCurlyError,@cscriptParenGroup,@Spell fold
else
    syn region      cscriptBlock            start=/{/   end=/}/     transparent fold
endif
"-----------------}


"-Numbers---------{
syn case ignore

syn match       cscriptNumbers          display transparent /\<\d\|\.\d/ contains=cscriptNumber,cscriptFloat,cscriptOctal,cscriptOctalErr
syn match       cscriptNumber           display contained /\d\+\>/

" hexadecimal
syn match       cscriptNumber           display contained /0x\x\+\>/

" flag the first zero of an octal number as something special
syn match       cscriptOctal            display contained /0\o\+\>/
syn match       cscriptOctalZero        display contained /\<0/

" floating point number, with dot, optional exponent
syn match       cscriptFloat            display contained /\d\+\.\d*\%(e[-+]\=\d\+\)\=/
" floating point number, starting with a dot, optional exponent
syn match       cscriptFloat            display contained /\.\d\+\%(e[-+]\=\d\+\)\>/
" floating point number, without dot, with exponent
syn match	cscriptFloat		display contained "\d\+e[-+]\=\d\+\>"
" hexadecimal floating point number, optional leading digits, with dot, with exponent
syn match	cscriptFloat		display contained "0x\x*\.\x\+p[-+]\=\d\+\>"
" hexadecimal floating point number, with leading digits, optional dot, with exponent
syn match	cscriptFloat		display contained "0x\x\+\.\=p[-+]\=\d\+\>"

" flag an octal number with wrong digits
syn match	cscriptOctalError       display contained "0\o*[89]\d*"

syn case match
"-----------------}


"-Tables----------{
syn region      cscriptTableBlock       transparent matchgroup=cscriptTable start="{" end="}" contains=TOP,cscriptStatement
"-----------------}


"-Arrays----------{
syn region      cscriptArrayBlock       transparent matchgroup=cscriptArray start="\[" end="]" contains=TOP,cscriptStatement
"-----------------}


"-Errors and Parens-{
syn cluster     cscriptParenGroup       contains=cscriptParenError,cscriptSpecial,@cscriptCommentGroup,cscriptCommentStartError,cscriptNumber,cscriptFloat,cscriptOctal
syn region      cscriptParen            transparent start=/(/ end=/)/ contains=ALLBUT,@cscriptParenGroup,cscriptBlock,cscriptParenError,@Spell
syn match       cscriptParenError       display /[\])]/
syn match       cscriptErrorInParen     display contained /[\]{}]/
syn match       cscriptError            /\<\%(else\|in\)\>/
"------------------}


" Statements
hi def link cscriptStatement            Statement
" Labels
hi def link cscriptLabel                Label
" Conditionals
hi def link cscriptConditional          Conditional
" Functions and Identifiers
hi def link cscriptFunction             Function
hi def link cscriptMetaMethod           Function
" Loops
hi def link cscriptRepeat               Repeat
" Comments
hi def link cscriptTodo                 Todo
hi def link cscriptCommentStart         Comment
hi def link cscriptComment              Comment
" Structured Data
hi def link cscriptTable                Structure
hi def link cscriptArray                Structure
" Constant Values
hi def link cscriptConstant             Constant
hi def link cscriptCharacter            Character
hi def link cscriptNumber               Number
hi def link cscriptOctal                Number
hi def link cscriptOctalZero            PreProc
hi def link cscriptFloat                Float
" Operators
hi def link cscriptOperator             Operator
hi def link cscriptSymbolOperator       Operator
" Errors
hi def link cscriptErrorInParen         cscriptError
hi def link cscriptOctalError           cscriptError
hi def link cscriptCommentError         cscriptError
hi def link cscriptCommentStartError    cscriptError
hi def link cscriptWrongComTail	        cscriptError
hi def link cscriptError                Error
" Identifiers
hi def link cscriptFunc                 Identifier


let b:current_syntax = "cst"

let &cpo = s:cpo_save
unlet s:cpo_save
" vim: et ts=8 sw=2
