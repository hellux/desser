" Vim syntax file
" Language: Desser
" License: Vim

syn keyword dsrStmt constrain debug
syn keyword dsrConditional if then else
syn keyword dsrLoop for in repeat
syn keyword dsrLet let const
syn keyword dsrDef def nextgroup=dsrStruct skipwhite
syn keyword dsrProp addr skip align baddr boffset bskip balign le be
syn match dsrProp "\(^\|[^']\)\<offset\>"
syn keyword dsrConstraint final constraint eq neq lt gt leq geq zero nonzero
syn match dsrStruct "\h\w*" display contained

syn keyword dsrPrim char bitvec bit u8 u16 u32 u64 i8 i16 i32 i64 f32 f64

syn match dsrAttribute display /\>'\(start\|size\|end\|offset\|length\)\>/
syn match dsrBin display "\<0b[01_]\+\>"
syn match dsrOct display "\<0o[0-7_]*+\>"
syn match dsrDec display "\<[0-9][0-9_]*\>"
syn match dsrHex display "\<0x[0-9a-f_]*\>"
syn keyword dsrSuper super
syn match dsrEscape display contained /\\\([nrt0\\'"]\|x\x\{2}\)/
syn region dsrString start=+"+ end=+"+ contains=dsrEscape

syn match dsrDelimiter "\["
syn match dsrDelimiter "\]"
syn match dsrDelimiter ","
syn match dsrDelimiter ";"
syn match dsrDelimiter "("
syn match dsrDelimiter ")"
syn match dsrDelimiter "\."

" comments
syn keyword dsrTodo TODO display contained
syn keyword dsrTodo XXX display contained
syn keyword dsrTodo FIXME display contained
syn match dsrCommentLine display "//.*" contains=dsrTodo
syn region dsrCommentBlock matchgroup=dsrCommentBlock start="/\*" end="\*/" display contains=dsrTodo,dsrCommentBlockNest
syn region dsrCommentBlockNest matchgroup=dsrCommentBlock start="/\*" end="\*/" display contains=dsrTodo,dsrCommentBlockNest contained transparent

hi def link dsrLet Keyword
hi def link dsrStmt Keyword
hi def link dsrConditional Keyword
hi def link dsrLoop Keyword
hi def link dsrProp Keyword
hi def link dsrConstraint Keyword
hi def link dsrDef Keyword
hi def link dsrStruct Identifier
hi def link dsrPrim Type
hi def link dsrAttribute Type

hi def link dsrDelimiter Delimiter

hi def link dsrBin Number
hi def link dsrOct Number
hi def link dsrDec Number
hi def link dsrHex Number
hi def link dsrSuper Constant

hi def link dsrString String
hi def link dsrEscape Special

hi def link dsrCommentLine Comment
hi def link dsrCommentBlock Comment
hi def link dsrTodo Todo

syn sync minlines=200
syn sync maxlines=500

let b:current_syntax = "desser"
