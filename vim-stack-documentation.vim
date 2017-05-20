function! HoogleSearch(word)
    let command = 'stack hoogle ' . a:word
    let job = job_start(command, {'in_io': 'null', 'close_cb': 'Handler'})
endfunction

function! Hoogle()
  let word = input("Search: ")
  call HoogleSearch(word)
endfunction

function! HoogleSearchCursor()
    let word = expand("<cword>")
    call HoogleSearch(word)
endfunction

function! Handler(channel)
    vnew
    setlocal nobuflisted buftype=nofile bufhidden=wipe noswapfile
    while ch_status(a:channel, {'part': 'out'}) == 'buffered'
        call append(line('$'),  ch_read(a:channel))
    endwhile
endfunction


function! ShowHaskellDocumentation()
    let word = expand("<cword>")
    let command = 'stack exec vim-haskell-doc ' . word
    let job = job_start(command, {'in_io': 'null', 'close_cb': 'Handler'})
endfunction

function! OpenHaskellDocumentation()
    let word = expand("<cword>")
    let command = 'stack exec vim-haskell-doc -- -u ' . word
    let job = job_start(command, {'in_io': 'null', 'callback': 'OpenLinkHandler'})
endfunction

function! OpenLinkHandler(channel, url)
    echo a:url
    let command = "firefox -new-tab " . a:url
    let job = job_start(command, {'callback': 'NotifyOpenHandler'})
endfunction

function! NotifyOpenHandler(channel, salida)
    call append(line('$'),  a:salida)
    echo "Documentation opened"
endfunction
