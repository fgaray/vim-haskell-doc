# Haskell documentation tool


This is a little tool to quickly access Haddock documentation of Haskell
functions in the command line and in VIM.


Just install this Haskell program with stack using "stack install" and then add
into your vimrc file this:

```
source DIR/vim-stack-documentation.vim
```

where DIR is the directory where you cloned this repo.

You can add maps for HoogleSearch, ShowHaskellDocumentation and
OpenHaskellDocumentation in your vimrc. For example:

```
nmap <Leader>h :call ShowHaskellDocumentation()<CR>
nmap <Leader>b :call OpenHaskellDocumentation()<CR>
nmap <Leader>hs :call HoogleSearchCursor()<CR>
nmap <Leader>hh :call Hoogle()<CR>
```


# TODO

* Create a propper vim plugin compatible with vundle.
