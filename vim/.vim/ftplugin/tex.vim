function! <SID>MakeLabelsList()
    grep! '\\label\{.*?\}'
    copen
endfunction
command! Labels silent call <SID>MakeLabelsList() | redraw!
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal tabstop=2
