" Author: Samuele Giraudo
" Creation: 2024-07
" Modifications: 2024-07

" Syntax file of the Llama language.
" This file has to be at ~/.vim/syntax/llm.vim

if exists("b:current_syntax")
    finish
endif

" Turns off spell checking.
set nospell

" Comments.
syn region Comment start="{" end="}" containedin=ALL

" Structure keyword and symbols.
syn match Delimiter "="
syn match Delimiter "\."
syn match Delimiter "!"

" Other symbols.
syn match Label "("
syn match Label ")"
syn match Label "\["
syn match Label "\]"
syn match Label "@"
syn match Operator "|"
syn match Operator "#"
syn match Operator "&"

" Variable names.
syn match Character "%\%([a-zA-Z0-9\-\_\./]\+\)"

" Constant names.
syn match Function "\'\%([a-zA-Z0-9\-\_\./]\+\)"

" Misc.
syn match Delimiter "/"
syn match Label "\.\."

