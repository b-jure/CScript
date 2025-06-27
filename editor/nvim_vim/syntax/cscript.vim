" CScript syntax file
" Language:     CScript 1.0
" Maintainer:   Jure Bagić <jurebagic99@gmail.com>
" Last Change:  2025 Mar 7
" Options:      cscript_version = 1
"               cscript_subversion = 0 (for 1.0)


" quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

syn clear

" keep in sync with ftplugin/cscript.vim
if !exists("cscript_version")
  " Default is CScript 1.0
  let cscript_version = 1
  let cscript_subversion = 0
elseif !exists("cscript_subversion")
  " cscript_version exists, but cscript_subversion doesn't.
  " In this case set it to 0.
  let cscript_subversion = 0
endif

" case sensitive
syn case match

" sync method
syn sync minlines=1000


"-Metamethods-----{
syn keyword cscriptMetaMethod __getidx __setidx __gc __close __call __init
syn keyword cscriptMetaMethod __concat __mod __pow __add __sub __mul __div
syn keyword cscriptMetaMethod __shl __shr __band __bor __bxor __unm __bnot
syn keyword cscriptMetaMethod __eq __lt __le __name
"-----------------}

"-Basic library---{{
syn keyword cscriptFunc error assert gc load loadfile runfile getmetalist
syn keyword cscriptFunc setmetalist nextfield pairs ipairs pcall xpcall
syn keyword cscriptFunc print warn len rawequal rawget rawset getargs
syn keyword cscriptFunc tonum tostr typeof getclass getsuper range
syn keyword cscriptFunc __G __ENV __VERSION __MT
"-Package library-}{
syn keyword cscriptFunc import
syn match cscriptFunc /\<package\.loadlib\>/
syn match cscriptFunc /\<package\.searchpath\>/
syn match cscriptFunc /\<package\.preload\>/
syn match cscriptFunc /\<package\.cpath\>/
syn match cscriptFunc /\<package\.path\>/
syn match cscriptFunc /\<package\.searchers\>/
syn match cscriptFunc /\<package\.loaded\>/
"-String library--}{
syn match cscriptFunc /\<string\.split\>/
syn match cscriptFunc /\<string\.rsplit\>/
syn match cscriptFunc /\<string\.startswith\>/
syn match cscriptFunc /\<string\.reverse\>/
syn match cscriptFunc /\<string\.repeat\>/
syn match cscriptFunc /\<string\.join\>/
syn match cscriptFunc /\<string\.fmt\>/
syn match cscriptFunc /\<string\.toupper\>/
syn match cscriptFunc /\<string\.tolower\>/
syn match cscriptFunc /\<string\.find\>/
syn match cscriptFunc /\<string\.rfind\>/
syn match cscriptFunc /\<string\.span\>/
syn match cscriptFunc /\<string\.cscap\>/
syn match cscriptFunc /\<string\.replace\>/
syn match cscriptFunc /\<string\.substr\>/
syn match cscriptFunc /\<string\.swapcase\>/
syn match cscriptFunc /\<string\.swapupper\>/
syn match cscriptFunc /\<string\.swaplower\>/
syn match cscriptFunc /\<string\.byte\>/
syn match cscriptFunc /\<string\.bytes\>/
syn match cscriptFunc /\<string\.char\>/
syn match cscriptFunc /\<string\.cmp\>/
syn match cscriptFunc /\<string\.ascii_uppercase\>/
syn match cscriptFunc /\<string\.ascii_lowercase\>/
syn match cscriptFunc /\<string\.ascii_letters\>/
syn match cscriptFunc /\<string\.digits\>/
syn match cscriptFunc /\<string\.hexdigits\>/
syn match cscriptFunc /\<string\.octdigits\>/
syn match cscriptFunc /\<string\.punctuation\>/
syn match cscriptFunc /\<string\.whitespace\>/
syn match cscriptFunc /\<string\.printable\>/
"-Math library----}{
syn match cscriptFunc /\<math\.abs\>/
syn match cscriptFunc /\<math\.acos\>/
syn match cscriptFunc /\<math\.atan\>/
syn match cscriptFunc /\<math\.ceil\>/
syn match cscriptFunc /\<math\.cos\>/
syn match cscriptFunc /\<math\.deg\>/
syn match cscriptFunc /\<math\.exp\>/
syn match cscriptFunc /\<math\.toint\>/
syn match cscriptFunc /\<math\.floor\>/
syn match cscriptFunc /\<math\.fmod\>/
syn match cscriptFunc /\<math\.ult\>/
syn match cscriptFunc /\<math\.log\>/
syn match cscriptFunc /\<math\.max\>/
syn match cscriptFunc /\<math\.min\>/
syn match cscriptFunc /\<math\.modf\>/
syn match cscriptFunc /\<math\.rad\>/
syn match cscriptFunc /\<math\.sin\>/
syn match cscriptFunc /\<math\.sqrt\>/
syn match cscriptFunc /\<math\.tan\>/
syn match cscriptFunc /\<math\.type\>/
syn match cscriptFunc /\<math\.srand\>/
syn match cscriptFunc /\<math\.rand\>/
syn match cscriptFunc /\<math\.randf\>/
syn match cscriptFunc /\<math\.pi\>/
syn match cscriptFunc /\<math\.huge\>/
syn match cscriptFunc /\<math\.maxint\>/
syn match cscriptFunc /\<math\.minint\>/
"-I/O library-----}{
syn match cscriptFunc /\<io\.open\>/
syn match cscriptFunc /\<io\.close\>/
syn match cscriptFunc /\<io\.flush\>/
syn match cscriptFunc /\<io\.input\>/
syn match cscriptFunc /\<io\.output\>/
syn match cscriptFunc /\<io\.popen\>/
syn match cscriptFunc /\<io\.tmpfile\>/
syn match cscriptFunc /\<io\.type\>/
syn match cscriptFunc /\<io\.lines\>/
syn match cscriptFunc /\<io\.read\>/
syn match cscriptFunc /\<io\.write\>/
syn match cscriptFunc /\<io\.stdin\>/
syn match cscriptFunc /\<io\.stdout\>/
syn match cscriptFunc /\<io\.stderr\>/
"-OS library------}{
syn match cscriptFunc /\<os\.clock\>/
syn match cscriptFunc /\<os\.date\>/
syn match cscriptFunc /\<os\.difftime\>/
syn match cscriptFunc /\<os\.execute\>/
syn match cscriptFunc /\<os\.exit\>/
syn match cscriptFunc /\<os\.getenv\>/
syn match cscriptFunc /\<os\.setenv\>/
syn match cscriptFunc /\<os\.remove\>/
syn match cscriptFunc /\<os\.rename\>/
syn match cscriptFunc /\<os\.setlocale\>/
syn match cscriptFunc /\<os\.time\>/
syn match cscriptFunc /\<os\.tmpname\>/
"-Regex library---}{
syn match cscriptFunc /\<reg\.find\>/
syn match cscriptFunc /\<reg\.match\>/
syn match cscriptFunc /\<reg\.gmatch\>/
syn match cscriptFunc /\<reg\.gsub\>/
"-Debug library---}{
syn match cscriptFunc /\<debug\.debug\>/
syn match cscriptFunc /\<debug\.getuservalue\>/
syn match cscriptFunc /\<debug\.gethook\>/
syn match cscriptFunc /\<debug\.getinfo\>/
syn match cscriptFunc /\<debug\.getlocal\>/
syn match cscriptFunc /\<debug\.getctable\>/
syn match cscriptFunc /\<debug\.getclist\>/
syn match cscriptFunc /\<debug\.getupvalue\>/
syn match cscriptFunc /\<debug\.upvaluejoin\>/
syn match cscriptFunc /\<debug\.upvalueid\>/
syn match cscriptFunc /\<debug\.setuservalue\>/
syn match cscriptFunc /\<debug\.sethook\>/
syn match cscriptFunc /\<debug\.setlocal\>/
syn match cscriptFunc /\<debug\.setupvalue\>/
syn match cscriptFunc /\<debug\.traceback\>/
syn match cscriptFunc /\<debug\.stackinuse\>/
syn match cscriptFunc /\<debug\.cstacklimit\>/
syn match cscriptFunc /\<debug\.maxstack\>/
"-List library----}{
syn match cscriptFunc /\<list\.len\>/
syn match cscriptFunc /\<list\.insert\>/
syn match cscriptFunc /\<list\.remove\>/
syn match cscriptFunc /\<list\.move\>/
syn match cscriptFunc /\<list\.new\>/
syn match cscriptFunc /\<list\.flatten\>/
syn match cscriptFunc /\<list\.concat\>/
syn match cscriptFunc /\<list\.sort\>/
syn match cscriptFunc /\<list\.maxindex\>/
"-----------------}}

"-Operators-------{
syn keyword cscriptOperator and or
syn match cscriptSymbolOperator /[<>=~^\*&|/%+-\!]\|\.{2,3}/
"-----------------}

"-Comments--------{
syn keyword cscriptTodo contained TODO FIXME XXX
syn cluster cscriptCommentGroup contains=cscriptTodo,cscriptDocTag
" single line
syn region cscriptComment matchgroup=cscriptCommentStart start=/#/ skip=/\\$/ end=/$/ keepend contains=@cscriptCommentGroup
syn region cscriptComment matchgroup=cscriptCommentStart start="///" skip=/\\$/ end=/$/ keepend contains=@cscriptCommentGroup
" multi-line
syn region cscriptComment matchgroup=cscriptCommentStart start=/\/\*/ end=/\*\// contains=@cscriptCommentGroup,cscriptCommentStartError fold extend
syn match cscriptDocTag display contained /\s\zs@\k\+/
" errors
syn match cscriptCommentError display /\*\//
syn match cscriptCommentStartError display /\/\*/me=e-1 contained
syn match cscriptWrongComTail display /\*\//
"-----------------}

"-Special---------{
" highlight \e (aka \x1b)
syn match cscriptSpecialEsc contained /\\e/
" highlight control chars
syn match cscriptSpecialControl contained /\\[\\abtnvfr'"]/
" highlight decimal escape sequence \ddd
syn match cscriptSpecialDec contained /\\[[:digit:]]\{1,3}/
" highlight hexadecimal escape sequence \xhh
syn match cscriptSpecialHex contained /\\x[[:xdigit:]]\{2}/
" highlight utf8 \u{xxxxxxxx} or \u[xxxxxxxx]
syn match cscriptSpecialUtf contained /\\u\%({[[:xdigit:]]\{1,8}}\|\[[[:xdigit:]]\{1,8}\]\)/
syn cluster cscriptSpecial contains=cscriptSpecialEsc,cscriptSpecialControl,cscriptSpecialDec,cscriptSpecialHex,cscriptSpecialUtf
" errors
syn match cscriptSpecialEscError /\\e/
syn match cscriptSpecialControlError /\\[\\abtnvfr'"]/
syn match cscriptSpecialDecError /\\[[:digit:]]\{3}/
syn match cscriptSpecialHexError /\\x[[:xdigit:]]\{2}/
syn match cscriptSpecialUtfError /\\u\%({[[:xdigit:]]\{1,8}}\|\[[[:xdigit:]]\{1,8}\]\)/
"-----------------}

"-Characters-----{
syn match cscriptCharacter /'\([^\\']\|\\[\\abtnvfr'"]\|\\x[[:xdigit:]]\{2}\)'/ contains=cscriptSpecialControl,cscriptSpecialHex
"-----------------}

"-Numbers---------{
" decimal integers
syn match cscriptNumber /\<\%(0\|[1-9][[:digit:]_]*\)\>/
" hexadecimal integers
syn match cscriptNumber /\<0x\x[[:xdigit:]_]*\>/
" octal integers
syn match cscriptOctal /\<0\o[0-7_]*\>/ contains=cscriptOctalZero
" flag the first zero of an octal number as something special
syn match cscriptOctalZero contained /\<0/

" decimal floating point number, with dot, optional exponent
syn match cscriptFloat /\<\d[[:digit:]_]*\.\d*\%([eE][-+]\=\d[[:digit:]_]*\)\=\>/
" decimal floating point number, starting with a dot, optional exponent
syn match cscriptFloat /\.\d\+\%([eE][-+]\=\d[[:digit:]_]*\)\>/
" decimal floating point number, without dot, with exponent
syn match cscriptFloat /\<\d[_0-9]*[eE][-+]\=\d[[:digit:]_]*\>/
" hexadecimal foating point number, optional leading digits, with dot, with exponent
syn match cscriptFloat /\<0[xX]\x[[:xdigit:]_]*\.\x\+[pP][-+]\=\d[[:digit:]_]*\>/
" hexadecimal floating point number, with leading digits, optional dot, with exponent
syn match cscriptFloat /\<0x\x[[:digit:]_]*\.\=[pP][-+]\=\d[[:digit:]_]*\>/

" flag an octal number with wrong digits
syn match cscriptOctalError contained /0\o*[89]\d*/
"-----------------}

"-Identifier------{
syn match cscriptIdentifier /\<\h\w*\>/
"-----------------}

"-Keywords--------{
syn keyword cscriptStatement break return continue
syn keyword cscriptConditional if else
syn keyword cscriptLabel case default switch
syn keyword cscriptRepeat loop while for
syn keyword cscriptConstant true false nil inf infinity
"-----------------}

"-Blocks----------{
if exists("c_curly_error")
    syn match cscriptCurlyError /}/
    syn region cscriptBlock start=/{/ end=/}/ contains=TOP,cscriptCurlyError,cscriptSpecialError,@Spell fold
else
    syn region cscriptBlock start=/{/ end=/}/ transparent fold
endif
"-----------------}

"-Parens---------{
syn region cscriptParen transparent start=/(/ end=/)/ contains=TOP,cscriptErrorInParen,cscriptLabel,cscriptRepeat,@Spell
syn match cscriptErrorInParen /)/
"---------------}

"-Bracket-------{
syn region cscriptBracket transparent matchgroup=cscriptBracket start="\[" end="]" contains=TOP,cscriptErrorInBracket,cscriptLabel,cscriptRepeat,@Spell
syn match cscriptErrorInBracket /]/
"---------------}

"-Strings---------{
syn region cscriptString start=/"/ skip=/\\"/ end=/"/ contains=@cscriptSpecial,@Spell
syn region cscriptLongString start=/\[\z(=\+\)\[/ end=/\]\z1\]/ contains=@Spell
"-----------------}

"-Foreach---------{
syn region cscriptForEach transparent matchgroup=cscriptRepeat start=/\<foreach\>\ze\_s\+\%(\h\w*\%(,\_s*\h\w*\)*\)\_s\<in\>/ end=/\h\w*\_s\+\zs\<in\>/me=e-2 contains=TOP,cscriptInError skipwhite skipempty
syn keyword cscriptForEach contained containedin=cscriptForEach in
syn match cscriptInError /\<in\>/
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
