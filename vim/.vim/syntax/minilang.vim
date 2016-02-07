" Vim syntax file
" Language:   Minilang
" Maintainer: Jacob Thomas Errington <minilang@mail.jerrington.me>

syn case match

syn keyword minType        int float string
syn keyword minDeclaration var
syn keyword minConditional if then else endif
syn keyword minRepeat      while do done
syn keyword minIO          print read

syn match   minIdent       /\<\I\i+\>/
syn match	minFloat       /\<\d\+\.\>/
syn match   minFloat       /\<\.\d\+\>/
syn match   minFloat       /\<\d\+\.\d\+\>/
syn match   minInt         /\<\d\+\>/

syn region  minBlock       start=/then/ end=/endif/ contains=minBlock
syn region  minBlock       start=/do/ end=/done/ contains=minBlock
syn region  minString      start=/"/ end=/"/

hi def link minType        Type
hi def link minDeclaration Keyword
hi def link minConditional Conditional
hi def link minRepeat      Repeat
hi def link minIO          Operator
hi def link minIdent       Identifier
hi def link minFloat       Float
hi def link minInt         Number
hi def link minString      String
