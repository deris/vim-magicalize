" operator-magicalize - Operator to convert several pattern to magic pattern
" Version: 0.0.1
" Author: deris0126 <deris0126@gmail.com>
" Copyright (C) 2014 @deris0126
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}

" Interface "{{{1
function! operator#magicalize#magicalize(motion_wise) "{{{2
  return s:operator_magicalize(a:motion_wise, function('magicalize#magicalize'))
endfunction
"}}}

function! operator#magicalize#verymagicalize(motion_wise) "{{{2
  return s:operator_magicalize(a:motion_wise, function('magicalize#verymagicalize'))
endfunction
"}}}

function! operator#magicalize#nomagicalize(motion_wise) "{{{2
  return s:operator_magicalize(a:motion_wise, function('magicalize#nomagicalize'))
endfunction
"}}}

function! operator#magicalize#verynomagicalize(motion_wise) "{{{2
  return s:operator_magicalize(a:motion_wise, function('magicalize#verynomagicalize'))
endfunction
"}}}

"}}}

" Private "{{{1
function! s:operator_magicalize(motion_wise, func) "{{{2
  let visual_command = s:visual_command_from_wise_name(a:motion_wise)

  let put_command = (s:deletion_moves_the_cursor_p(
    \                    a:motion_wise,
    \                    getpos("']")[1:2],
    \                    len(getline("']")),
    \                    [line('$'), len(getline('$'))]
    \                  )
    \                  ? 'p'
    \                  : 'P')

  if !s:is_empty_region(getpos("'["), getpos("']"))
    let original_selection = &g:selection
    let &g:selection = 'inclusive'

    let reg = 'z'
    let [save_reg, save_type] = [getreg(reg), getregtype(reg)]
    execute 'normal!' '`['.visual_command.'`]"zd'
    let text = getreg(reg)

    let @z = a:func(text)

    execute 'normal!' '"'.reg.put_command

    call setreg(reg, save_reg, save_type)

    let &g:selection = original_selection
  end
  return
endfunction
"}}}

" function! s:deletion_moves_the_cursor_p(motion_wise) "{{{2
function! s:deletion_moves_the_cursor_p(motion_wise,
\                                       motion_end_pos,
\                                       motion_end_last_col,
\                                       buffer_end_pos)
  let [buffer_end_line, buffer_end_col] = a:buffer_end_pos
  let [motion_end_line, motion_end_col] = a:motion_end_pos

  if a:motion_wise ==# 'char'
    return ((a:motion_end_last_col == motion_end_col)
    \       || (buffer_end_line == motion_end_line
    \           && buffer_end_col <= motion_end_col))
  elseif a:motion_wise ==# 'line'
    return buffer_end_line == motion_end_line
  elseif a:motion_wise ==# 'block'
    return 0
  else
    echoerr 'E2: Invalid wise name:' string(a:wise_name)
    return 0
  endif
endfunction
"}}}

function! s:is_empty_region(begin, end) "{{{2
  " Whenever 'operatorfunc' is called, '[ is always placed before '] even if
  " a backward motion is given to g@.  But there is the only one exception.
  " If an empty region is given to g@, '[ and '] are set to the same line, but
  " '[ is placed after '].
  return a:begin[1] == a:end[1] && a:end[2] < a:begin[2]
endfunction
"}}}

function! s:visual_command_from_wise_name(wise_name) "{{{2
  if a:wise_name ==# 'char'
    return 'v'
  elseif a:wise_name ==# 'line'
    return 'V'
  elseif a:wise_name ==# 'block'
    return "\<C-v>"
  else
    echoerr 'E1: Invalid wise name:' string(a:wise_name)
    return 'v'  " fallback
  endif
endfunction
"}}}

"}}}


" restriction
" * 改行有りの文字列はとりあえず受け付けない
" * パターンが正しいことを前提とする

" __END__  "{{{1
" vim: foldmethod=marker
