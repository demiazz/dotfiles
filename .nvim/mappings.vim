" Leader key

  let mapleader = ","

" Clear search results in normal mode

  nnoremap <silent> <Esc><Esc> :nohlsearch<CR><Esc>

" Switch splits

  nmap <C-h> <C-W>h
  nmap <C-j> <C-W>j
  nmap <C-k> <C-W>k
  nmap <C-l> <C-W>l

" Relative windows

  nmap <Leader><Left>  :leftabove vnew<CR>
  nmap <Leader><Right> :rightbelow vnew<CR>
  nmap <Leader><Up>    :leftabove new<CR>
  nmap <Leader><Down>  :rightbelow new<CR>

" Buffers

  nmap <Leader>bl :ls<CR>:b
  nmap <Leader>bp :bp<CR>
  nmap <Leader>bn :bn<CR>

" Tabs

  nmap <Tab>   gt
  nmap <S-Tab> gT

" Disable arrow keys

  inoremap <Up>    <NOP>
  inoremap <Down>  <NOP>
  inoremap <Left>  <NOP>
  inoremap <Right> <NOP>

  noremap  <Up>    <NOP>
  noremap  <Down>  <NOP>
  noremap  <Left>  <NOP>
  noremap  <Right> <NOP>

  imap <C-h> <C-o>h
  imap <C-j> <C-o>j
  imap <C-k> <C-o>k
  imap <C-l> <C-o>l

" Ctrl + s

  map <C-s>  <Esc>:w<CR>
  imap <C-s> <Esc>:w<CR>
