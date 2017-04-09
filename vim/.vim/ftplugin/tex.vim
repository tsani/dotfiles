function! <SID>MakeLabelsList()
    grep! '\\label\{.*?\}'
    copen
endfunction
command! Labels silent call <SID>MakeLabelsList() | redraw!
