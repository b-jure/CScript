" CScript syntax file
" Language:     CScript 1.0
" Maintainer:   Jure Bagić <jurebagic99@gmail.com>
" Last Change:  2025 Mar 7


" quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim


syn clear


" case sensitive
syn case match
syn sync fromstart

"-Operators-------{
syn keyword cscriptOperator         and or
syn match   cscriptSymbolOperator   /[<>=~^\*&|/%+-\!]\|\.{2,3}/
"-----------------}

"-Comments--------{
syn keyword cscriptTodo         contained TODO FIXME XXX
syn cluster cscriptCommentGroup contains=cscriptTodo,cscriptDocTag
" single line
syn region  cscriptComment  matchgroup=cscriptCommentStart start=/#/ skip=/\\$/ end=/$/ keepend contains=@cscriptCommentGroup
syn region  cscriptComment  matchgroup=cscriptCommentStart start="///" skip=/\\$/ end=/$/ keepend contains=@cscriptCommentGroup
" multi-line
if exists("c_no_comment_fold")
    syn region  cscriptComment  matchgroup=cscriptCommentStart start=/\/\*/ end=/\*\// contains=@cscriptCommentGroup,cscriptCommentStartError extend
else
    syn region  cscriptComment  matchgroup=cscriptCommentStart start=/\/\*/ end=/\*\// contains=@cscriptCommentGroup,cscriptCommentStartError fold extend
endif
syn match   cscriptDocTag               display contained /\s\zs@\k\+/
" errors
syn match   cscriptCommentError         display /\*\//
syn match   cscriptCommentStartError    display /\/\*/me=e-1 contained
syn match   cscriptWrongComTail	        display /\*\//
"-----------------}

"-Special---------{
" highlight \e (aka \x1b)
syn match   cscriptSpecialEsc       contained /\\e/
" highlight control chars
syn match   cscriptSpecialControl   contained /\\[\\abtnvfr'"]/
" highlight decimal escape sequence \ddd
syn match   cscriptSpecialDec       contained /\\[[:digit:]]\{1,3}/
" highlight hexadecimal escape sequence \xhh
syn match   cscriptSpecialHex       contained /\\x[[:xdigit:]]\{2}/
" highlight utf8 \u{xxxxxxxx} or \u[xxxxxxxx]
syn match   cscriptSpecialUtf       contained /\\u\%({[[:xdigit:]]\{1,8}}\|\[[[:xdigit:]]\{1,8}\]\)/
syn cluster cscriptSpecial  contains=cscriptSpecialEsc,cscriptSpecialControl,cscriptSpecialDec,cscriptSpecialHex,cscriptSpecialUtf
" errors
syn match   cscriptSpecialEscError      /\\e/
syn match   cscriptSpecialControlError  /\\[\\abtnvfr'"]/
syn match   cscriptSpecialDecError      /\\[[:digit:]]\{3}/
syn match   cscriptSpecialHexError      /\\x[[:xdigit:]]\{2}/
syn match   cscriptSpecialUtfError      /\\u\%({[[:xdigit:]]\{1,8}}\|\[[[:xdigit:]]\{1,8}\]\)/
"-----------------}

"-Characters-----{
syn match   cscriptCharacter    /'\([^\\']\|\\[\\abtnvfr'"]\|\\x[[:xdigit:]]\{2}\)'/ contains=cscriptSpecialControl,cscriptSpecialHex
"-----------------}

"-Numbers---------{
syn case ignore
syn match   cscriptNumbers      transparent /\<\d\|\.\d/ contains=cscriptNumber,cscriptFloat,cscriptOctal,cscriptOctalError,cscriptConstant
" decimal integers
syn match   cscriptNumber       contained /\%(0\|[^0\d]\d*\)\>/
" hexadecimal integers
syn match   cscriptNumber       contained /0x\x\+\>/
" octal integers
syn match   cscriptOctal        contained /0\o\+\>/ contains=cscriptOctalZero
" flag the first zero of an octal number as something special
syn match   cscriptOctalZero    contained /\<0/
" decimal floating point number, with dot, optional exponent
syn match   cscriptFloat        contained /\d\+\.\d*\%(e[-+]\=\d\+\)\=/
" decimal floating point number, starting with a dot, optional exponent
syn match   cscriptFloat        contained /\.\d\+\%(e[-+]\=\d\+\)\>/
" decimal floating point number, without dot, with exponent
syn match   cscriptFloat        contained "\d\+e[-+]\=\d\+\>"
" hexadecimal foating point number, optional leading digits, with dot, with exponent
syn match   cscriptFloat        contained "0x\x*\.\x\+p[-+]\=\d\+\>"
" hexadecimal floating point number, with leading digits, optional dot, with exponent
syn match   cscriptFloat        contained "0x\x\+\.\=p[-+]\=\d\+\>"
" flag an octal number with wrong digits
syn match   cscriptOctalError   contained "0\o*[89]\d*"
syn case match
"-----------------}

"-Identifier------{
syn match cscriptIdentifier /\<\h\w*\>/
"-----------------}

"-Keywords--------{
syn keyword     cscriptStatement        break return continue
syn keyword     cscriptConditional      if else
syn keyword     cscriptLabel            case default switch
syn keyword     cscriptRepeat           loop while for
syn keyword     cscriptConstant         true false nil inf infinity
"-----------------}

"-Blocks----------{
if exists("c_curly_error")
    syn match   cscriptCurlyError   /}/
    syn region  cscriptBlock        start=/{/ end=/}/ contains=TOP,cscriptCommentStartError,cscriptCurlyError,cscriptSpecialError,cscriptErrorInBracket,@Spell fold
else
    syn region   cscriptBlock       start=/{/ end=/}/ transparent fold
endif
"-----------------}

"-Parens---------{
syn cluster cscriptParenGroup   contains=@cscriptSpecial,@cscriptCommentGroup,cscriptCommentStartError,cscriptOctalZero,cscriptNumber,cscriptFloat,cscriptOctal,cscriptOctalError
syn region  cscriptParen        transparent start=/(/ end=/)/ contains=ALLBUT,cscriptAttribute,cscriptIf,cscriptLabel,cscriptRepeat,@cscriptParenGroup,@Spell
syn match   cscriptErrorInParen display contained /]/
"---------------}

"-Bracket-------{
syn region  cscriptBracket  transparent matchgroup=cscriptBracket start="\[" end="]" contains=TOP,cscriptIf,@cscriptParenGroup,cscriptLabel,cscriptRepeat,cscriptClass,@Spell
syn match   cscriptErrorInBracket   display contained /]/
"---------------}

"-Strings---------{
syn region cscriptString start=/"/ skip=/\\"/ end=/"/ contains=@cscriptSpecial,@Spell
syn region cscriptLongString start=/\[\z(=\+\)\[/ end=/\]\z1\]/ contains=@Spell
"-----------------}

"-Foreach---------{
syn region  cscriptForEach  transparent matchgroup=cscriptRepeat start=/\<foreach\>\ze\_s\+\%(\h\w*\%(,\_s*\h\w*\)*\)\_s\<in\>/ end=/\h\w*\_s\+\zs\<in\>/me=e-2 contains=TOP,cscriptInError skipwhite skipempty
syn keyword cscriptForEach  contained containedin=cscriptForEach in
syn match   cscriptInError  /\<in\>/
"-----------------}

"-MetaMethods-----{
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
syn keyword     cscriptFunc             rawset getresults tonumber tostring typeof
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
syn match       cscriptFunc             /\<string\.fmt\>/
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

syn match cscriptSemicolon /;/
syn match cscriptComma /,/

syn region cscriptLocalStatement transparent start=/\<local\_s*\h\w*\_s*/ end=/\ze\%(;\|=\|{\)/ contains=cscriptComma,cscriptLocal,cscriptAttribute,cscriptClassDefinition,cscriptFunction,cscriptFn,cscriptFunctionCall
syn keyword cscriptLocal local contained
syn match cscriptAttribute /<\_s*\%(close\|final\)\_s*>/ contained

"-Classes---------{
syn region cscriptClassDefinition transparent matchgroup=cscriptStatement start=/\<class\>/ end=/{/me=e-1 contains=cscriptClass skipwhite skipempty
syn keyword cscriptClass class inherits
syn keyword cscriptSuper super
"-----------------}

syn region cscriptFunction transparent start=/\<fn\_s\+\k\+\%(\.\k\+\)*\%(\_s*(\zs\)\@=/ end=/)/ keepend contains=cscriptFn,cscriptFunctionCall
syn match cscriptFunctionCall /\k\+\%(\_s*(\)\@=/ keepend
syn keyword cscriptFn fn


hi def link cscriptAttribute            StorageClass
hi def link cscriptFn                   cscriptStatement
hi def link cscriptSemicolon            cscriptStatement
hi def link cscriptLocal                cscriptStatement
hi def link cscriptIdentifier           NONE
hi def link cscriptFunction             cscriptStatement
hi def link cscriptSuper                PreProc
hi def link cscriptClassDefinition      cscriptStatement
hi def link cscriptClass                cscriptStatement
hi def link cscriptMetaMethod           cscriptFunc
hi def link cscriptFunc                 Identifier
hi def link cscriptFunctionCall         Identifier
hi def link cscriptDocTag               Underlined
hi def link cscriptForEach              cscriptRepeat
hi def link cscriptSpecialEsc           SpecialChar
hi def link cscriptSpecialControl       SpecialChar
hi def link cscriptSpecialDec           SpecialChar
hi def link cscriptSpecialHex           SpecialChar
hi def link cscriptSpecialUtf           SpecialChar
hi def link cscriptString               String
hi def link cscriptLongString           String
hi def link cscriptCharacter            Character
hi def link cscriptStatement            Statement
hi def link cscriptLabel                Label
hi def link cscriptConditional          Conditional
hi def link cscriptRepeat               Repeat
hi def link cscriptTodo                 Todo
hi def link cscriptCommentStart         Comment
hi def link cscriptNumber               Number
hi def link cscriptOctal                Number
hi def link cscriptOctalZero            PreProc
hi def link cscriptFloat                Float
hi def link cscriptSymbolOperator       cscriptOperator
hi def link cscriptOperator             Operator
hi def link cscriptComment              Comment
hi def link cscriptConstant             Constant
hi def link cscriptCurlyError           cscriptError
hi def link cscriptErrorInParen         cscriptError
hi def link cscriptErrorInBracket       cscriptError
hi def link cscriptOctalError           cscriptError
hi def link cscriptCommentError         cscriptError
hi def link cscriptCommentStartError    cscriptError
hi def link cscriptWrongComTail	        cscriptError
hi def link cscriptSpecialEscError      cscriptError
hi def link cscriptSpecialControlError  cscriptError
hi def link cscriptSpecialDecError      cscriptError
hi def link cscriptSpecialHexError      cscriptError
hi def link cscriptSpecialUtfError      cscriptError 
hi def link cscriptInError              cscriptError
hi def link cscriptError                Error

let &cpo = s:cpo_save
unlet s:cpo_save

let b:current_syntax = "cscript"
" vim: et ts=8 sw=2
