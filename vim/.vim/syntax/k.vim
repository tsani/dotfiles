" Vim syntax file
" Language: k
" Maintainer: Victor Janas (victor.janas@1010data.com)
" Last Change: 2015 July
" TODO operators with adverbs
" TODO \d: match which ignores a0:1
" TODO dots preceding words
" k system stuff
syn keyword kSystemFunction _log _exp _abs _sqr _sqrt _floor _dot _mul _inv _sin _cos _tan _asin _acos _atan _sinh _cosh _tanh _lsq _draw _jd _dj _ltime _lt _gtime _in _bin _lin _binl _dv _dvl _di _dil _sv _vs _ci _ic _sm _ss _ssr _bd _db _getenv _setenv _host _size _exit _f
syn keyword kConstant _t _d _n _p _s _i _h _w _u _a _k _T
syn keyword kConditional if
syn keyword kRepeat  while do
syn keyword kTodo  contained TODO FIXME XXX HACK
" Symbol matching, complete with ridiculous logic that allows for \" inside of quoted symbols
syntax match kTokens display "`\.\=\h\(\w\|\.\)*"
syntax match kTokens display "\v`\".{-}\\@<!\""
syn match kWords display "\.\=\h\(\w\|\.\)*"
syn match kColons display "\d\:"
" String and Character constants
" Highlight special characters (those which have a backslash) differently
syn match kSpecial display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
syn match kSpecial display contained "\\\(u\x\{4}\|U\x\{8}\)"
syn region kString  start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=kSpecial,@Spell
syn match kNumbers display transparent "\<\d\|\.\d" contains=kNumber,kFloat
" Same, but without octal error (for comments)
syn match kNumbersCom display contained transparent "\<\d\|\.\d" contains=kNumber,kFloat
syn match kNumber  display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"
syn match kFloat  display contained "\d\+f"
"floating point number, with dot, optional exponent
syn match kFloat  display contained "\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\="
"floating point number, starting with a dot, optional exponent
syn match kFloat  display contained "\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
"floating point number, without dot, with exponent
syn match kFloat  display contained "\d\+e[-+]\=\d\+[fl]\=\>"

syn match kOperator display "[\+\-\_\,\.\?\!\@\#\$\%\^\&\*\~\=\|]"
syn match kOperator  /[/\']/
syn match kOperator  /[/\']:/
syntax region kSlash start="^\\" end=/$/ contains=kComment
syntax region kComment start="^/" end="$" contains=kTodo
syntax region kComment start="\s/" end="$" contains=kTodo


syn match kSeparator ";"

"catch errrors
syn match kInvalidFunction "\<_[a-zA-Z][a-zA-Z0-9]*\>"
syn region kBlock	transparent start='\[' end='\]' contains=ALLBUT,kBlockError
syn match  kBlockError	"\]"
syn region kSet	transparent start='{' end='}' contains=ALLBUT,kSetError
syn match  kSetError	"}"
 
" Define the default highlighting.
" Only used when an item doesn't have highlighting yet
hi def link kTokens  Identifier
hi def link kNumber Number
hi def link kFloat  Number
hi def link kConstant  Structure
hi def link kSystemFunction Structure
hi def link kColons  Error
hi def link kSpecial Special
"hi def link kWords  Identifier
hi def link kConditional Conditional
hi def link kRepeat  Repeat
hi def link kOperator  Operator
hi def link kSlash  PreProc 
hi def link kString  String
hi def link kComment  Comment
hi def link kTodo  Todo
hi link kSeparator Type
  hi link kInvalidFunction 	kError
  hi link kBlockError		kError
  hi link kSetError		kError
  hi link kError		Error
  hi link kWords Normal

let b:current_syntax = "k"
" vim: ts=8
 
 


