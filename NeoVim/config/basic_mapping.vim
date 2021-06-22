" ===
" === Basic Mappings
" ===
" Set <LEADER> as <SPACE>, ; as :
let mapleader=" "

" Save & quit
noremap Q :wq<CR>
noremap S :w<CR>

" Quit every mode
inoremap jk <Esc>
inoremap JK <Esc>la
inoremap <C-g> <Esc>

vnoremap jk <Esc>
vnoremap <C-g> <Esc>

" Indentation
nnoremap < <<
nnoremap > >>

" Search
noremap <LEADER><CR> :nohlsearch<CR>

" ===
" === Window management
" ===
" Use <space> + new arrow keys for moving the cursor around windows
noremap <LEADER>w <C-w>w
noremap <LEADER>k <C-w>k
noremap <LEADER>j <C-w>j
noremap <LEADER>h <C-w>h
noremap <LEADER>l <C-w>l

" Disable the default s key
"noremap s <nop>

" split the screens to up (horizontal), down (horizontal), left (vertical), right (vertical)
"noremap sk :set nosplitbelow<CR>:split<CR>:set splitbelow<CR>
"noremap sj :set splitbelow<CR>:split<CR>
"noremap sh :set nosplitright<CR>:vsplit<CR>:set splitright<CR>
"noremap sl :set splitright<CR>:vsplit<CR>

" Resize splits with arrow keys
noremap <leader><up> :res +5<CR>
noremap <leader><down> :res -5<CR>
noremap <leader><left> :vertical resize-5<CR>
noremap <leader><right> :vertical resize+5<CR>

" Place the two screens up and down
"noremap sh <C-w>t<C-w>K
" Place the two screens side by side
"noremap sv <C-w>t<C-w>H

" Rotate screens
"noremap srh <C-w>b<C-w>K
"noremap srv <C-w>b<C-w>H

" Press <SPACE> + q to close the window below the current window
noremap <LEADER>q <C-w>j:q<CR>

" ===
" === Tab management
" ===
" Create a new tab with tu
noremap tu :tabe<CR>
" Move around tabs with tn and ti
noremap tn :-tabnext<CR>
noremap ti :+tabnext<CR>
" Move the tabs with tmn and tmi
noremap tmn :-tabmove<CR>
noremap tmi :+tabmove<CR>

" ===
" === Other useful stuff
" ===
" Opening a terminal window
noremap <LEADER>/ :set splitbelow<CR>:split<CR>:res +10<CR>:term<CR>

" Press space twice to jump to the next '<++>' and edit it
noremap <LEADER><LEADER> <Esc>/<++><CR>:nohlsearch<CR>c4l

" Spelling Check with <space>sc
noremap <LEADER>sc :set spell!<CR>

" Press ` to change case (instead of ~)
noremap ` ~

" Auto change directory to current dir
autocmd BufEnter * silent! lcd %:p:h

" Call figlet
noremap tx :r !figlet

" find and replace
noremap \s :%s//g<left><left>

" set wrap
noremap <LEADER>sw :set wrap<CR>

" Call figlet
noremap tx :r !figlet
