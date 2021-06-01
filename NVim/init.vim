" ===
" === Auto load for first time uses
" ===
if empty(glob('~/.config/nvim/autoload/plug.vim'))
	silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
				\ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" ===
" === Create a _machine_specific.vim file to adjust machine specific stuff, like python interpreter location
" ===
let has_machine_specific_file = 1
if empty(glob('~/.config/nvim/_machine_specific.vim'))
	let has_machine_specific_file = 0
	silent! exec "!cp ~/.config/nvim/default_configs/_machine_specific_default.vim ~/.config/nvim/_machine_specific.vim"
endif
source ~/.config/nvim/_machine_specific.vim

" === Editor Setup ===
" ====================
source ~/.config/nvim/config/basic_setting.vim

" ===
" === Basic Mappings
" ===
source ~/.config/nvim/config/basic_mapping.vim

" Compile Function
source ~/.config/nvim/config/compile_function.vim

" === package management ===
source ~/.config/nvim/config/package_list.vim
source ~/.config/nvim/config/plug_config.vim

" ===
" === Markdown Settings ===
" ===
source ~/.config/nvim/config/md_snippets.vim

" ===
" === Dress up my vim
" ===
set termguicolors " enable true colors support
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
colors deus
hi NonText ctermfg=gray guifg=grey10

" ===
" === Airline
" ===
let g:airline_powerline_fonts = 1

" ===
" === Necessary Commands to Execute
" ===
exec "nohlsearch"


" Open the _machine_specific.vim file if it has just been created
if has_machine_specific_file == 0
	exec "e ~/.config/nvim/_machine_specific.vim"
endif

