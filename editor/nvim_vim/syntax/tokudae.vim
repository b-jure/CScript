" Tokudae syntax file
" Language:     Tokudae 1.0
" Maintainer:   Jure Bagić <jurebagic99@gmail.com>
" Last Change:  2025 Jul 4
" Options:      tokudae_version = 1
"               tokudae_subversion = 0 (for 1.0)


" quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

syn clear

" keep in sync with ftplugin/tokudae.vim
if !exists("tokudae_version")
  " Default is Tokudae 1.0
  let tokudae_version = 1
  let tokudae_subversion = 0
elseif !exists("tokudae_subversion")
  " tokudae_version exists, but tokudae_subversion doesn't.
  " In this case set it to 0.
  let tokudae_subversion = 0
endif

" case sensitive
syn case match

" sync method
syn sync minlines=1000

"-Comments--------{
syn keyword tokudaeTodo contained TODO FIXME XXX
syn cluster tokudaeCommentGroup contains=tokudaeTodo,tokudaeDocTag,@Spell
" single line
syn region tokudaeComment matchgroup=tokudaeCommentStart start=/#/ skip=/\\$/ end=/$/ keepend contains=@tokudaeCommentGroup
syn region tokudaeComment matchgroup=tokudaeCommentStart start="///" skip=/\\$/ end=/$/ keepend contains=@tokudaeCommentGroup
" multi-line
syn region tokudaeComment matchgroup=tokudaeCommentStart start=/\/\*/ end=/\*\// contains=@tokudaeCommentGroup,tokudaeCommentStartError fold extend
syn match tokudaeDocTag display contained /\s\zs@\k\+/
" errors
syn match tokudaeCommentError display /\*\//
syn match tokudaeCommentStartError display /\/\*/me=e-1 contained
syn match tokudaeWrongComTail display /\*\//
"-----------------}

"-Special---------{
" highlight \e (aka \x1b)
syn match tokudaeSpecialEsc contained /\\e/
" highlight control chars
syn match tokudaeSpecialControl contained /\\[\\abtnvfr'"]/
" highlight decimal escape sequence \ddd
syn match tokudaeSpecialDec contained /\\[[:digit:]]\{1,3}/
" highlight hexadecimal escape sequence \xhh
syn match tokudaeSpecialHex contained /\\x[[:xdigit:]]\{2}/
" highlight utf8 \u{xxxxxxxx} or \u[xxxxxxxx]
syn match tokudaeSpecialUtf contained /\\u\%({[[:xdigit:]]\{1,8}}\|\[[[:xdigit:]]\{1,8}\]\)/
syn cluster tokudaeSpecial contains=tokudaeSpecialEsc,tokudaeSpecialControl,tokudaeSpecialDec,tokudaeSpecialHex,tokudaeSpecialUtf
" errors
syn match tokudaeSpecialEscError /\\e/
syn match tokudaeSpecialControlError /\\[\\abtnvfr'"]/
syn match tokudaeSpecialDecError /\\[[:digit:]]\{3}/
syn match tokudaeSpecialHexError /\\x[[:xdigit:]]\{2}/
syn match tokudaeSpecialUtfError /\\u\%({[[:xdigit:]]\{1,8}}\|\[[[:xdigit:]]\{1,8}\]\)/
"-----------------}

"-Characters-----{
syn match tokudaeCharacter /'\([^\\']\|\\[\\abtnvfr'"]\|\\x[[:xdigit:]]\{2}\|\\[[:digit:]]\{1,3}\)'/
"-----------------}

"-Numbers---------{
" decimal integers
syn match tokudaeNumber /\<[0-9][[:digit:]_]*\>/
" hexadecimal integers
syn match tokudaeNumber /\<0x\x[[:xdigit:]_]*\>/
" binary integers
syn match tokudaeNumber /\<0[bB][0-1][0-1_]*\>/
" octal integers
syn match tokudaeOctal /\<0\o[0-7_]*\>/ contains=tokudaeOctalZero
" flag the first zero of an octal number as something special
syn match tokudaeOctalZero contained /\<0/

" decimal floating point number, with dot, optional exponent
syn match tokudaeFloat /\<\d[[:digit:]_]*\.\d*\%([eE][-+]\=\d[[:digit:]_]*\)\=\>/
" decimal floating point number, starting with a dot, optional exponent
syn match tokudaeFloat /\.\d\+\%([eE][-+]\=\d[[:digit:]_]*\)\=\>/
" decimal floating point number, without dot, with exponent
syn match tokudaeFloat /\<\d[_0-9]*[eE][-+]\=\d[[:digit:]_]*\>/
" hexadecimal foating point number, optional leading digits, with dot, with exponent
syn match tokudaeFloat /\<0[xX]\x[[:xdigit:]_]*\.\x\+[pP][-+]\=\d[[:digit:]_]*\>/
" hexadecimal floating point number, with leading digits, optional dot, with exponent
syn match tokudaeFloat /\<0x\x[[:digit:]_]*\.\=[pP][-+]\=\d[[:digit:]_]*\>/
"-----------------}

"-Keywords--------{
syn keyword tokudaeStatement break return continue fn local
syn keyword tokudaeConditional if else
syn keyword tokudaeLabel case default switch
syn keyword tokudaeRepeat loop while for
syn keyword tokudaeConstant true false nil inf infinity
"-----------------}

"-Blocks----------{
syn region tokudaeBlock transparent fold start=/{/ end=/}/ contains=TOP,tokudaeCurlyError
syn match tokudaeCurlyError /}/
"-----------------}

"-Parens---------{
syn region tokudaeParen transparent start=/(/ end=/)/ contains=TOP,tokudaeErrorInParen
syn match tokudaeErrorInParen /)/
"---------------}

"-Bracket-------{
syn region tokudaeBracket transparent start=/\[/ end=/]/ contains=TOP,tokudaeErrorInBracket
syn match tokudaeErrorInBracket /]/
"---------------}

"-Strings---------{
syn region tokudaeString start=/"/ skip=/\\"/ end=/"/ contains=@tokudaeSpecial,@Spell
syn region tokudaeLongString start=/\[\z(=\+\)\[/ end=/]\z1]/ contains=@Spell
"-----------------}

"-Identifier------{
syn match tokudaeIdentifier /\<[A-Za-z_]\w*\>/
"-----------------}

"-Foreach---------{
syn region tokudaeForEach transparent matchgroup=tokudaeRepeat start=/\<foreach\>/ end=/\<in\>/ contains=TOP,tokudaeIn,tokudaeInError
syn keyword tokudaeIn contained in
syn match tokudaeInError /\<in\>/
"-----------------}

"-Classes---------{
syn region tokudaeClassDefinition transparent matchgroup=tokudaeStatement start=/\<class\>/ end=/{/me=e-1 contains=tokudaeClass
syn keyword tokudaeClass class inherits
syn keyword tokudaeSuper super
"-----------------}

"-Operators-------{
syn keyword tokudaeOperator and or
syn match tokudaeSymbolOperator />\ze\%([^>=,]\|\n\)/
syn match tokudaeSymbolOperator />>\ze\%([^>=,]\|\n\)/
syn match tokudaeSymbolOperator /<\ze\%([^<=,]\|\n\)/
syn match tokudaeSymbolOperator /<<\ze\%([^<=,]\|\n\)/
syn match tokudaeSymbolOperator /\/\ze\%([^/=\*,]\|\n\)/
syn match tokudaeSymbolOperator /\/\/\ze\%([^/=\*,]\|\n\)/
syn match tokudaeSymbolOperator /\*\ze\%([^\*=,]\|\n\)/
syn match tokudaeSymbolOperator /\*\*\ze\%([^\*=,]\|\n\)/
syn match tokudaeSymbolOperator /&\ze\%([^&=,]\|\n\)/
syn match tokudaeSymbolOperator /|\ze\%([^|=,]\|\n\)/
syn match tokudaeSymbolOperator /\^\ze\%([^\^=,]\|\n\)/
syn match tokudaeSymbolOperator /\~\+\ze\%([^=,]\|\n\)/
syn match tokudaeSymbolOperator /!\+\ze\%([^=,]\|\n\)/
syn match tokudaeSymbolOperator /%\ze\%([^%=,]\|\n\)/
syn match tokudaeSymbolOperator /+\ze\%([^+=,]\|\n\)/
syn match tokudaeSymbolOperator /-\ze\%([^-=,]\|\n\)/
syn match tokudaeSymbolOperator /-\+\ze\%([^=,]\|\n\)/
syn match tokudaeSymbolOperator /=\ze\%([^=,]\|\n\)/
syn match tokudaeSymbolOperator /==\ze\%([^=,]\|\n\)/
syn match tokudaeSymbolOperator /!=\ze\%([^=,]\|\n\)/
syn match tokudaeSymbolOperator /<=\ze\%([^=,]\|\n\)/
syn match tokudaeSymbolOperator />=\ze\%([^=,]\|\n\)/
syn match tokudaeSymbolOperator /\.\.\ze\%([^\.=,]\|\n\)/
syn match tokudaeSymbolOperator /\.\.\.\ze\%([^\.=,]\|\n\)/
syn match tokudaeSymbolOperator /,/
syn match tokudaeSymbolOperator /?\ze\%([^?=]\|\n\)/
" compound assignments
syn match tokudaeSymbolOperator /+=\ze\%([^,=+&%|\/><\*\.-]\|\n\)/
syn match tokudaeSymbolOperator /-=\ze\%([^,=+&%|\/><\*\.-]\|\n\)/
syn match tokudaeSymbolOperator /*=\ze\%([^,=+&%|\/><\*\.-]\|\n\)/
syn match tokudaeSymbolOperator /%=\ze\%([^,=+&%|\/><\*\.-]\|\n\)/
syn match tokudaeSymbolOperator /\/=\ze\%([^,=+&%|\/><\*\.-]\|\n\)/
syn match tokudaeSymbolOperator /&=\ze\%([^,=+&%|\/><\*\.-]\|\n\)/
syn match tokudaeSymbolOperator /|=\ze\%([^,=+&%|\/><\*\.-]\|\n\)/
syn match tokudaeSymbolOperator /\/\/=\ze\%([^,=+&%|\/><\*\.-]\|\n\)/
syn match tokudaeSymbolOperator /\*\*=\ze\%([^,=+&%|\/><\*\.-]\|\n\)/
syn match tokudaeSymbolOperator /<<=\ze\%([^,=+&%|\/><\*\.-]\|\n\)/
syn match tokudaeSymbolOperator />>=\ze\%([^,=+&%|\/><\*\.-]\|\n\)/
syn match tokudaeSymbolOperator /\.\.=\ze\%([^,=+&%|\/><\*\.-]\|\n\)/
syn match tokudaeSymbolOperator /++\ze\%([^,=+&%|\/><\*\.-]\|\n\)/
syn match tokudaeSymbolOperator /--\ze\%([^,=+&%|\/><\*\.-]\|\n\)/
"-----------------}

"-Other----------_{
syn match tokudaeFunctionCall /\k\+\_s*(\@=/
syn match tokudaeSemicolon /;/
syn match tokudaeAttribute /<\_s*\%(close\|final\)\_s*>/
syn match tokudaeClosure /|\_s*|/
syn region tokudaeClosure transparent matchgroup=tokudaeStatement start=/|\ze\_s*\h\+\_s*\%(,\_s*\h\+\_s*\)*/ end=/|/
"-----------------}

"-Metamethods-----{
syn keyword tokudaeMetaTag __getidx __setidx __gc __close __call __init
syn keyword tokudaeMetaTag __concat __mod __pow __add __sub __mul __div
syn keyword tokudaeMetaTag __shl __shr __band __bor __bxor __unm __bnot
syn keyword tokudaeMetaTag __eq __lt __le __name __metatable
"-----------------}

"-Basic library---{{
syn keyword tokudaeFunc error assert gc load loadfile runfile getmetatable
syn keyword tokudaeFunc setmetatable getmethods setmethods nextfield pairs
syn keyword tokudaeFunc ipairs pcall xpcall print printf warn len rawequal
syn keyword tokudaeFunc rawget rawset getargs tonum tostr typeof getclass
syn keyword tokudaeFunc clone unwrapmethod getsuper range
syn keyword tokudaeFunc __POSIX __WINDOWS __G __ENV __VERSION
" metatag table keys
syn match tokudaeFunc /\<__MT>\ze[^\.]/
syn match tokudaeFunc /\<__MT\.getidx\>/
syn match tokudaeFunc /\<__MT\.setidx\>/
syn match tokudaeFunc /\<__MT\.gc\>/
syn match tokudaeFunc /\<__MT\.close\>/
syn match tokudaeFunc /\<__MT\.call\>/
syn match tokudaeFunc /\<__MT\.init\>/
syn match tokudaeFunc /\<__MT\.concat\>/
syn match tokudaeFunc /\<__MT\.mod\>/
syn match tokudaeFunc /\<__MT\.pow\>/
syn match tokudaeFunc /\<__MT\.add\>/
syn match tokudaeFunc /\<__MT\.sub\>/
syn match tokudaeFunc /\<__MT\.mul\>/
syn match tokudaeFunc /\<__MT\.div\>/
syn match tokudaeFunc /\<__MT\.shl\>/
syn match tokudaeFunc /\<__MT\.shr\>/
syn match tokudaeFunc /\<__MT\.band\>/
syn match tokudaeFunc /\<__MT\.bor\>/
syn match tokudaeFunc /\<__MT\.bxor\>/
syn match tokudaeFunc /\<__MT\.unm\>/
syn match tokudaeFunc /\<__MT\.bnot\>/
syn match tokudaeFunc /\<__MT\.eq\>/
syn match tokudaeFunc /\<__MT\.lt\>/
syn match tokudaeFunc /\<__MT\.le\>/
syn match tokudaeFunc /\<__MT\.name\>/
syn match tokudaeFunc /\<__MT\.metatable\>/
syn match tokudaeFunc /\<__MT\.tostring\>/
"-Package library-}{
syn keyword tokudaeFunc import
syn match tokudaeFunc /\<package\.loadlib\>/
syn match tokudaeFunc /\<package\.searchpath\>/
syn match tokudaeFunc /\<package\.preload\>/
syn match tokudaeFunc /\<package\.cpath\>/
syn match tokudaeFunc /\<package\.path\>/
syn match tokudaeFunc /\<package\.searchers\>/
syn match tokudaeFunc /\<package\.loaded\>/
"-String library--}{
syn match tokudaeFunc /\<string\.split\>/
syn match tokudaeFunc /\<string\.rsplit\>/
syn match tokudaeFunc /\<string\.startswith\>/
syn match tokudaeFunc /\<string\.reverse\>/
syn match tokudaeFunc /\<string\.repeat\>/
syn match tokudaeFunc /\<string\.join\>/
syn match tokudaeFunc /\<string\.fmt\>/
syn match tokudaeFunc /\<string\.toupper\>/
syn match tokudaeFunc /\<string\.tolower\>/
syn match tokudaeFunc /\<string\.find\>/
syn match tokudaeFunc /\<string\.rfind\>/
syn match tokudaeFunc /\<string\.span\>/
syn match tokudaeFunc /\<string\.cspan\>/
syn match tokudaeFunc /\<string\.replace\>/
syn match tokudaeFunc /\<string\.substr\>/
syn match tokudaeFunc /\<string\.swapcase\>/
syn match tokudaeFunc /\<string\.swapupper\>/
syn match tokudaeFunc /\<string\.swaplower\>/
syn match tokudaeFunc /\<string\.byte\>/
syn match tokudaeFunc /\<string\.bytes\>/
syn match tokudaeFunc /\<string\.char\>/
syn match tokudaeFunc /\<string\.cmp\>/
syn match tokudaeFunc /\<string\.ascii_uppercase\>/
syn match tokudaeFunc /\<string\.ascii_lowercase\>/
syn match tokudaeFunc /\<string\.ascii_letters\>/
syn match tokudaeFunc /\<string\.digits\>/
syn match tokudaeFunc /\<string\.hexdigits\>/
syn match tokudaeFunc /\<string\.octdigits\>/
syn match tokudaeFunc /\<string\.punctuation\>/
syn match tokudaeFunc /\<string\.whitespace\>/
syn match tokudaeFunc /\<string\.printable\>/
"-Math library----}{
syn match tokudaeFunc /\<math\.abs\>/
syn match tokudaeFunc /\<math\.acos\>/
syn match tokudaeFunc /\<math\.atan\>/
syn match tokudaeFunc /\<math\.ceil\>/
syn match tokudaeFunc /\<math\.cos\>/
syn match tokudaeFunc /\<math\.deg\>/
syn match tokudaeFunc /\<math\.exp\>/
syn match tokudaeFunc /\<math\.toint\>/
syn match tokudaeFunc /\<math\.floor\>/
syn match tokudaeFunc /\<math\.fmod\>/
syn match tokudaeFunc /\<math\.ult\>/
syn match tokudaeFunc /\<math\.log\>/
syn match tokudaeFunc /\<math\.max\>/
syn match tokudaeFunc /\<math\.min\>/
syn match tokudaeFunc /\<math\.modf\>/
syn match tokudaeFunc /\<math\.rad\>/
syn match tokudaeFunc /\<math\.sin\>/
syn match tokudaeFunc /\<math\.sqrt\>/
syn match tokudaeFunc /\<math\.tan\>/
syn match tokudaeFunc /\<math\.type\>/
syn match tokudaeFunc /\<math\.srand\>/
syn match tokudaeFunc /\<math\.rand\>/
syn match tokudaeFunc /\<math\.randf\>/
syn match tokudaeFunc /\<math\.pi\>/
syn match tokudaeFunc /\<math\.huge\>/
syn match tokudaeFunc /\<math\.maxint\>/
syn match tokudaeFunc /\<math\.minint\>/
"-I/O library-----}{
syn match tokudaeFunc /\<io\.open\>/
syn match tokudaeFunc /\<io\.close\>/
syn match tokudaeFunc /\<io\.flush\>/
syn match tokudaeFunc /\<io\.input\>/
syn match tokudaeFunc /\<io\.output\>/
syn match tokudaeFunc /\<io\.popen\>/
syn match tokudaeFunc /\<io\.tmpfile\>/
syn match tokudaeFunc /\<io\.type\>/
syn match tokudaeFunc /\<io\.lines\>/
syn match tokudaeFunc /\<io\.read\>/
syn match tokudaeFunc /\<io\.write\>/
syn match tokudaeFunc /\<io\.stdin\>/
syn match tokudaeFunc /\<io\.stdout\>/
syn match tokudaeFunc /\<io\.stderr\>/
"-OS library------}{
syn match tokudaeFunc /\<os\.clock\>/
syn match tokudaeFunc /\<os\.date\>/
syn match tokudaeFunc /\<os\.difftime\>/
syn match tokudaeFunc /\<os\.execute\>/
syn match tokudaeFunc /\<os\.exit\>/
syn match tokudaeFunc /\<os\.getenv\>/
syn match tokudaeFunc /\<os\.setenv\>/
syn match tokudaeFunc /\<os\.remove\>/
syn match tokudaeFunc /\<os\.rename\>/
syn match tokudaeFunc /\<os\.setlocale\>/
syn match tokudaeFunc /\<os\.time\>/
syn match tokudaeFunc /\<os\.tmpname\>/
"-Regex library---}{
syn match tokudaeFunc /\<reg\.find\>/
syn match tokudaeFunc /\<reg\.match\>/
syn match tokudaeFunc /\<reg\.gmatch\>/
syn match tokudaeFunc /\<reg\.gsub\>/
"-Debug library---}{
syn match tokudaeFunc /\<debug\.debug\>/
syn match tokudaeFunc /\<debug\.getuservalue\>/
syn match tokudaeFunc /\<debug\.gethook\>/
syn match tokudaeFunc /\<debug\.getinfo\>/
syn match tokudaeFunc /\<debug\.getlocal\>/
syn match tokudaeFunc /\<debug\.getctable\>/
syn match tokudaeFunc /\<debug\.getclist\>/
syn match tokudaeFunc /\<debug\.getupvalue\>/
syn match tokudaeFunc /\<debug\.upvaluejoin\>/
syn match tokudaeFunc /\<debug\.upvalueid\>/
syn match tokudaeFunc /\<debug\.setuservalue\>/
syn match tokudaeFunc /\<debug\.sethook\>/
syn match tokudaeFunc /\<debug\.setlocal\>/
syn match tokudaeFunc /\<debug\.setupvalue\>/
syn match tokudaeFunc /\<debug\.traceback\>/
syn match tokudaeFunc /\<debug\.stackinuse\>/
syn match tokudaeFunc /\<debug\.cstacklimit\>/
syn match tokudaeFunc /\<debug\.maxstack\>/
"-List library----}{
syn match tokudaeFunc /\<list\.len\>/
syn match tokudaeFunc /\<list\.enumerate\>/
syn match tokudaeFunc /\<list\.insert\>/
syn match tokudaeFunc /\<list\.remove\>/
syn match tokudaeFunc /\<list\.move\>/
syn match tokudaeFunc /\<list\.new\>/
syn match tokudaeFunc /\<list\.flatten\>/
syn match tokudaeFunc /\<list\.concat\>/
syn match tokudaeFunc /\<list\.sort\>/
syn match tokudaeFunc /\<list\.shrink\>/
syn match tokudaeFunc /\<list\.isordered\>/
syn match tokudaeFunc /\<list\.maxindex\>/
"-UTF8 library----}{
syn match tokudaeFunc /\<utf8\.offset\>/
syn match tokudaeFunc /\<utf8\.codepoint\>/
syn match tokudaeFunc /\<utf8\.char\>/
syn match tokudaeFunc /\<utf8\.len\>/
syn match tokudaeFunc /\<utf8\.codes\>/
syn match tokudaeFunc /\<utf8\.charpattern\>/
"-----------------}}

hi def link tokudaeClosure              tokudaeStatement 
hi def link tokudaeAttribute            StorageClass
hi def link tokudaeSemicolon            tokudaeStatement
hi def link tokudaeIdentifier           NONE
hi def link tokudaeSuper                PreProc
hi def link tokudaeClassDefinition      tokudaeStatement
hi def link tokudaeClass                tokudaeStatement
hi def link tokudaeMetaTag              tokudaeFunc
hi def link tokudaeFunc                 Identifier
hi def link tokudaeFunctionCall         Identifier
hi def link tokudaeDocTag               Underlined
hi def link tokudaeForEach              tokudaeRepeat
hi def link tokudaeSpecialEsc           SpecialChar
hi def link tokudaeSpecialControl       SpecialChar
hi def link tokudaeSpecialDec           SpecialChar
hi def link tokudaeSpecialHex           SpecialChar
hi def link tokudaeSpecialUtf           SpecialChar
hi def link tokudaeString               String
hi def link tokudaeLongString           String
hi def link tokudaeCharacter            Character
hi def link tokudaeStatement            Statement
hi def link tokudaeLabel                Label
hi def link tokudaeConditional          Conditional
hi def link tokudaeRepeat               Repeat
hi def link tokudaeTodo                 Todo
hi def link tokudaeCommentStart         Comment
hi def link tokudaeNumber               Number
hi def link tokudaeOctal                Number
hi def link tokudaeOctalZero            PreProc
hi def link tokudaeFloat                Float
hi def link tokudaeSymbolOperator       tokudaeOperator
hi def link tokudaeOperator             Operator
hi def link tokudaeComment              Comment
hi def link tokudaeConstant             Constant
hi def link tokudaeCurlyError           tokudaeError
hi def link tokudaeErrorInParen         tokudaeError
hi def link tokudaeErrorInBracket       tokudaeError
hi def link tokudaeCommentError         tokudaeError
hi def link tokudaeCommentStartError    tokudaeError
hi def link tokudaeWrongComTail	        tokudaeError
hi def link tokudaeSpecialEscError      tokudaeError
hi def link tokudaeSpecialControlError  tokudaeError
hi def link tokudaeSpecialDecError      tokudaeError
hi def link tokudaeSpecialHexError      tokudaeError
hi def link tokudaeSpecialUtfError      tokudaeError 
hi def link tokudaeInError              tokudaeError
hi def link tokudaeError                Error

let &cpo = s:cpo_save
unlet s:cpo_save

let b:current_syntax = "tokudae"
" vim: et ts=8 sw=2
