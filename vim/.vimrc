"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => pathogen invocation
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

runtime bundle/vim-pathogen/autoload/pathogen.vim

execute pathogen#infect()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Filetype associations
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Fix LaTeX file extension association
autocmd BufNewFile,BufRead *.tex setf tex

" Minilang
autocmd BufNewFile,BufRead *.min setf minilang

" Tutch
autocmd BufNewFile,BufRead *.tut setf tutch


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Quickfix list hacking
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Capture the output of a command into the quickfix list.
" Only really useful if the command is a compiler or linter of some kind, with
" output in the format that Vim expects in the quickfix list.
" Realistically, for compilers though, we should use set makeprg in a filetype
" plugin.
command! -bang -nargs=1 Q cexpr<bang> system('$SHELL -c ' . shellescape(<q-args>))


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Syntastic setup
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" Disable haskell-vim omnifunc
let g:haskellmode_completion_ghc = 0
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

" Enable Neocomplete
let g:neocomplete#enable_at_startup = 1

" Search using ag
set grepprg=ag\ --nogroup\ --filename

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? "\<C-y>" : "\<CR>"
endfunction

" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"

" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"

" Enable filetype plugins
filetype plugin indent on

" Sets how many lines of history VIM has to remember
set history=700

" Lines 80 characters or more are a sin
set tw=79

" Set to auto read when a file is changed from the outside
set autoread

" Use backslash to reverse a movement command.
nnoremap \ ,

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
let mapleader = ","
let g:mapleader = ","
let maplocalleader = "\\"

" Fast saving
nnoremap <leader>w :w!<cr>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Set 7 lines to the cursor - when moving vertically using j/k
set so=7

" Highlight the line that the cursor is on.
set cursorline

" Set a colored column on the column of the textwidth
set colorcolumn=+1

" Turn on the WiLd menu
set wildmenu
set wildmode=longest:full
set completeopt=longest,menuone

" Ignore compiled files
set wildignore=*.o,*~,*.pyc,*.hi

"Always show current position
set ruler

" Height of the command bar
set cmdheight=1

" A buffer becomes hidden when it is abandoned
set hid

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
set smartcase

" Highlight search results
set hlsearch

" Hijack search highlighting feature to simply cause a highlight of the word
" under the cursor.
nmap <leader>h yiw/<C-r>0<return><C-o>

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" For regular expressions turn magic on
set magic

" Show matching brackets when text indicator is over them
set showmatch

" How many tenths of a second to blink when matching brackets
set mat=2

" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" Line numbers: relative while in normal mode, no numbers in insert mode
set nonumber
set relativenumber

au InsertEnter * :set norelativenumber | set foldcolumn=4
au InsertLeave * :set relativenumber | set foldcolumn=0

" whitespace characters
set list
set listchars=tab:>\ ,trail:-,nbsp:+


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Enable syntax highlighting
syntax enable

set background=light
colorscheme solarized

" Set extra options when running in GUI mode
if has("gui_running")
    set guioptions-=T
    set guioptions+=e
    set t_Co=256
    set guitablabel=%M\ %t
endif

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8

" Use Unix as the standard file type
set ffs=unix,dos,mac

" Show trailing whitespace as an error
match Error /\s\+$/


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Neovim terminal emulator
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if has("nvim")
    " To quickly open split terminals
    command! -bang -nargs=* -complete=shellcmd Tv :vs | terminal<bang> <args>
    command! -bang -nargs=* -complete=shellcmd Th :sp | terminal<bang> <args>

    " Moving out of terminal buffers easily
    tmap <C-l> <C-\><C-n><C-l>
    tmap <C-h> <C-\><C-n><C-h>
    tmap <C-j> <C-\><C-n><C-j>
    tmap <C-k> <C-\><C-n><C-k>

    " Fast close terminal window
    tmap <C-\><C-q> <C-\><C-n>:q<CR>

    "" Automatically enter insert mode when entering a terminal buffer
    "" Turns out that I don't really like this behaviour, so I've commented it
    "" out. For completeness, I keep it in the file though.
    " autocmd WinEnter * call InsertTerminal()

    " Reset terminal size
    " Remark: we have to go to normal mode *twice* due to the WinEnter
    " autocommand that invokes InsertTerminal() (above)
    tmap <C-_> <C-\><C-n>:sp<CR><C-\><C-n>:q<CR>

    autocmd TermOpen * setf terminal
endif

function! InsertTerminal()
    if &buftype ==# 'terminal'
        startinsert
    endif
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Use spaces instead of tabs
set expandtab

" Be smart when using tabs ;)
set smarttab

" 1 tab == 4 spaces, except when it's not!
set shiftwidth=4
set tabstop=4

" Auto indent
set ai

" Wrap lines
set wrap

" Break lines
set linebreak


""""""""""""""""""""""""""""""
" => Visual mode related
""""""""""""""""""""""""""""""

" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
vnoremap <silent> * :call VisualSelection('f')<CR>
vnoremap <silent> # :call VisualSelection('b')<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Moving around, tabs, windows and buffers
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Treat long lines as break lines (useful when moving around in them)
nnoremap j gj
nnoremap k gk

" Remap space to 'repeat find'
nnoremap <space> ;

" Quickly open command window
nnoremap ; q:

" Open current file in another tab
nnoremap <C-t> :tabe %<CR>

" Disable highlight when <leader><cr> is pressed
nnoremap <silent> <leader><cr> :noh<cr>

" Close the current buffer
nnoremap <leader>bd :Bclose<cr>

" Close all the buffers
nnoremap <leader>ba :1,1000 bd!<cr>

" Useful mappings for managing tabs
nnoremap <leader>tn :tabnew<cr>
nnoremap <leader>to :tabonly<cr>
nnoremap <leader>tc :tabclose<cr>
nnoremap <leader>tm :tabmove

" To quickly move between tabs
nnoremap <leader>> :tabnext<cr>
nnoremap <leader>< :tabprevious<cr>

nnoremap <leader>bn :bnext<cr>
nnoremap <leader>bp :bprev<cr>

" Opens a new tab with the current buffer's path
" Super useful when editing files in the same directory
map <leader>te :tabedit <c-r>=expand("%:p:h")<cr>/

" Switch CWD to the directory of the open buffer
map <leader>cd :cd %:p:h<cr>:pwd<cr>

" Specify the behavior when switching between buffers
try
set switchbuf=usetab
set stal=2
catch
endtry

" Return to last edit position when opening files
autocmd BufReadPost *
 \ if line("'\"") > 0 && line("'\"") <= line("$") |
 \   exe "normal! g`\"" |
 \ endif
" Remember info about open buffers on close
set viminfo^=%

" Smart way to move between windows
nnoremap <C-j> <C-W>j
nnoremap <C-k> <C-W>k
nnoremap <C-h> <C-W>h
nnoremap <C-l> <C-W>l


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Code folding
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set foldmethod=indent "Fold code based on indentation
set foldnestmax=3     "Fold up to three levels in
set nofoldenable      "Don't fold by default


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Persistent undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Keep undo history across sessions, by storing in file.
" Only works all the time.
if has('persistent_undo')
    silent !mkdir -p ~/.vim/backups
    set undodir=~/.vim/backups
    set undofile
endif


""""""""""""""""""""""""""""""
" => Status line
""""""""""""""""""""""""""""""

" Always show the status line
set laststatus=2

" Format the status line
set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ %y\ \ Line:\ %l/%L\ Col:\ %c


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Editing mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Remap VIM 0 to first non-blank character
map 0 ^

" Move a line of text using ALT+[jk] or Comamnd+[jk] on mac
nmap <M-j> mz:m+<cr>`z
nmap <M-k> mz:m-2<cr>`z
vmap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vmap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

if has("mac") || has("macunix")
    nmap <D-j> <M-j>
    nmap <D-k> <M-k>
    vmap <D-j> <M-j>
    vmap <D-k> <M-k>
endif

" Inserting a single character
nnoremap s :exec "normal i".nr2char(getchar())."\e"<CR>

" Delete trailing white space on save, useful for Python and CoffeeScript ;)
func! DeleteTrailingWS()
exe "normal mz"
%s/\s\+$//ge
exe "normal `z"
endfunc
autocmd BufWrite *.py :call DeleteTrailingWS()
autocmd BufWrite *.coffee :call DeleteTrailingWS()
autocmd BufWrite *.hs :call DeleteTrailingWS()


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => vimgrep searching and cope displaying
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" When you press gv you vimgrep after the selected text
vnoremap <silent> gv :call VisualSelection('gv')<CR>

" Open grep the word under the cursor.
nnoremap <leader>g viw"gy:silent grep! <C-R>g<CR>:cope<CR>:redraw!<CR>

" Vimgreps in the current file
map <leader><space> :grep  <C-R>%<C-A><right><right><right><right><right>

" When you press <leader>r you can search and replace the selected text
vnoremap <silent> <leader>r :call VisualSelection('replace')<CR>

" Do :help cope if you are unsure what cope is. It's super useful!
"
" When you search with vimgrep, display your results in cope by doing:
"   <leader>cc
"
" To go to the next search result do:
"   <leader>n
"
" To go to the previous search results do:
"   <leader>p
"
map <leader>cc :botright cope<cr>
map <leader>co :%y<cr>:tabnew<cr>:set syntax=qf<cr>Pgg
map <leader>n :cn<cr>
map <leader>p :cp<cr>
map <leader>sw yw /<C-R>0<cr>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Spell checking
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Pressing ,ss will toggle and untoggle spell checking
map <leader>ss :setlocal spell!<cr>

" Shortcuts using <leader>
map <leader>sn ]s
map <leader>sp [s
map <leader>sa zg
map <leader>s? z=


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Misc
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Remove the Windows ^M - when the encodings gets messed up
noremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

" Quickly open a buffer for scripbble
nnoremap <leader>a :e ~/buffer<cr>

" Quickly close the current window.
nnoremap <leader>q :q<cr>

" Switch buffers
nnoremap <leader>z :b #<CR>

" make all cut/paste commands affect the clipboard
set clipboard=unnamed

nnoremap <leader>b :mak<CR>

" Make sure that we can jump to the beginning and end of the line in ex mode
cnoremap <C-A> <Home>
cnoremap <C-E> <End>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Helper functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

function! CmdLine(str)
    exe "menu Foo.Bar :" . a:str
    emenu Foo.Bar
    unmenu Foo
endfunction

function! VisualSelection(direction) range
    let l:saved_reg = @"
    execute "normal! vgvy"

    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'b'
        execute "normal ?" . l:pattern . "^M"
    elseif a:direction == 'gv'
        call CmdLine("vimgrep " . '/'. l:pattern . '/' . ' **/*.')
    elseif a:direction == 'replace'
        call CmdLine("%s" . '/'. l:pattern . '/')
    elseif a:direction == 'f'
        execute "normal /" . l:pattern . "^M"
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction


" Returns true if paste mode is enabled
function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    en
    return ''
endfunction

" Don't close window, when deleting a buffer
function! <SID>BufcloseCloseIt()
   let l:currentBufNum = bufnr("%")
   let l:alternateBufNum = bufnr("#")

   if buflisted(l:alternateBufNum)
     buffer #
   else
     bnext
   endif

   if bufnr("%") == l:currentBufNum
     new
   endif

   if buflisted(l:currentBufNum)
     execute("bdelete! ".l:currentBufNum)
   endif
endfunction
command! Bclose call <SID>BufcloseCloseIt()

" Search for the ... arguments separated with whitespace (if no '!'),
" or with non-word characters (if '!' added to command).
function! SearchMultiLine(bang, ...)
  if a:0 > 0
    let sep = (a:bang) ? '\_W\+' : '\_s\+'
    let @/ = join(a:000, sep)
  endif
endfunction
command! -bang -nargs=* -complete=tag S call SearchMultiLine(<bang>0, <f-args>)|normal! /<C-R>/<CR>
" e.g. `:S hello world` finds the string 'hello world' with the space being
" *any* whitespace character, including newlines. The function SearchMultiLine
" joins all the arguments (so the strings 'hello' and 'world') with the regex
" fragment '\_W\+' meaning 'one or more whitespace characters including
" newlines'. This forms a complete regex, which is saved to the register named
" '/'. Then, using 'normal!', the search is executed by typing a slash and
" typing out the contents of the register into the search line and hitting
" return.

""""""""""""""""""""'""""""""""""""""""""""""""""""""""""""
" => Haskell Mode
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" use ghc functionality for haskell files
" au Bufenter *.hs compiler ghc

" configure browser for haskell_doc.vim
let g:haddock_browser = "/usr/bin/firefox"
let g:haddock_docdir = "/home/tsani/.cabal/share/doc"


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Tricks
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Write current buffer as root.
command! Sw w !sudo tee % > /dev/null

try
" vim-commentary defines these, but it turns out they're actually deprecated,
" so let's unmap them since I don't use them anyway.
unmap \\
unmap \\\
unmap \\u
catch
endtry
