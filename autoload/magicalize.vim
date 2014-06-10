" magicalize - Convert several pattern to magic pattern
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

function! magicalize#magicalize(expr) "{{{2
  return s:convert_pattern(a:expr, '\m')
endfunction
"}}}

function! magicalize#verymagicalize(expr) "{{{2
  return s:convert_pattern(a:expr, '\v')
endfunction
"}}}

function! magicalize#nomagicalize(expr) "{{{2
  return s:convert_pattern(a:expr, '\M')
endfunction
"}}}

function! magicalize#verynomagicalize(expr) "{{{2
  return s:convert_pattern(a:expr, '\V')
endfunction
"}}}

"}}}

" Private "{{{1

let s:escaped_backslash     = '\%(^\|[^\\]\)\%(\\\\\)*'
let s:non_escaped_backslash = '\%(^\|[^\\]\)\%(\\\\\)*\\'

function! s:convert_pattern(expr, magic_type) "{{{2
  " divide magic parts
  let pattern_sets = s:divide_pattern(a:expr)
  " divide escaped pattern or not escaped pattern
  let result_pattern = ''
  for pattern_set in pattern_sets
    let magic_type = pattern_set[0]
    let pattern = pattern_set[1]

    if magic_type ==# '\v' ||
      \a:magic_type ==# '\v'
      let result_pattern .= s:convert_magicpattern(pattern, function('s:convert_verymagic'), [magic_type, a:magic_type])
    else
      let result_pattern .= s:convert_magicpattern(pattern, function('s:convert_except4verymagic'), [magic_type, a:magic_type])
    endif
  endfor

  return a:magic_type . result_pattern
endfunction
"}}}

function! s:divide_pattern(expr) "{{{2
  let patterns = split(a:expr, s:escaped_backslash.'\zs\ze\\[mvMV]')

  let pattern_set = []

  for pattern in patterns
    if pattern =~# '^\\[mvMV]'
      call add(pattern_set, split(pattern, '^\\[mMvV]\zs'))
    else
      call add(pattern_set, [ &magic ? '\m' : '\M', pattern ])
    endif
  endfor

  return pattern_set
endfunction
"}}}

function! s:convert_magicpattern(expr, funcescape, args) "{{{2
  let expr = a:expr
  let from = a:args[0]
  let to   = a:args[1]

  let esc = (from ==# '\m' || from ==# '\v' ? '' : '\\')

  let needescape   = [] " エスケープが必要な文字列リスト
  let noneedescape = [] " エスケープが不要な文字列リスト

  " エスケープが必要な文字列と不要な文字列を分ける
  " エスケープが不要な文字列は正規表現の[](括弧内に含まれる一文字にマッチ)の中身
  while (expr != '')
    " パターンについて(可読性のため3つに分割)
    " ・1つめのパターンは先頭から正規表現[]の前までの文字列
    "   [がエスケープされていることを考慮してエスケープされていない部分にマッチ
    " ・2つめのパターンは正規表現[]とその中身の文字列
    "   ]がエスケープされていることを考慮してエスケープされていない部分にマッチ
    " ・3つめのパターンは正規表現[]以降、終端までの文字列
    let list = matchlist(expr,
      \ '\(\%(^\|^.\{-}[^\\]\)\%(\\\\\)*\)'.
      \ '\(\%('.esc.'\|'.(from ==# '\v' ? '%' : '\\%').'\|'.'\\_\)\%('.s:escaped_backslash.'\\_\)\@<!\[.\{-}'.s:escaped_backslash.'\]\)'.
      \ '\(.*\)$')

    " マッチしなくなったら残りの文字列をエスケープ用の文字列リストに追加
    if empty(list)
      call add(needescape, expr)
      break
    endif

    let [beforestr, matchstr, afterstr] = list[1 : 3]
    if (from ==# '\M' || from ==# '\V') &&
      \(to ==# '\m' || to ==# '\v')
      let matchstr = substitute(matchstr, '^\\\(\[.*\]\)$', '\1', '')
    elseif (from ==# '\m' || from ==# '\v') &&
      \    (to ==# '\M' || to ==# '\V')
      let matchstr = substitute(matchstr, '^\(\[.*\]\)$', '\\\1', '')
    endif
    if from ==# '\v' &&
      \(to ==# '\m' || to ==# '\M' || to ==# '\V')
      let matchstr = substitute(matchstr, '^\(%\[.*\]\)$', '\\\1', '')
    elseif (from ==# '\m' || from ==# '\M' || from ==# '\V') &&
      \    to ==# '\v'
      let matchstr = substitute(matchstr, '^\\\(%\[.*\]\)$', '\1', '')
    endif

    call add(needescape, beforestr)
    call add(noneedescape, matchstr)

    " 残りの文字列をさらにパターンマッチ
    let expr = afterstr
  endwhile

  " エスケープが必要な文字列をエスケープ
  let escaped = []
  for str in needescape
    call add(escaped, a:funcescape(str, a:args))
  endfor

  " エスケープ後の文字列リストをマージして文字列に戻す
  let mergedstr = s:merge_list2string(escaped, noneedescape)

  return mergedstr
endfunction
"}}}

" 文字列リストをマージして文字列に変換
" [list1[0], list2[0], list1[1], list2[1], ...]のようにマージ
function! s:merge_list2string(list1, list2) "{{{2
  let mergedstr = ''
  let i = 0
  let len = len(a:list1) < len(a:list2) ? len(a:list1) : len(a:list2)
  while i < len
    let mergedstr .= a:list1[i]
    let mergedstr .= a:list2[i]

    let i += 1
  endwhile

  let tail = len(a:list1) < len(a:list2) ? a:list2[i :] : a:list1[i :]

  for str in tail
    let mergedstr .= str
  endfor

  return mergedstr
endfunction
"}}}

" verymagic形式のパターンとそれ以外の形式のパターンを相互変換する
function! s:convert_verymagic(expr, args) "{{{2
  let from = a:args[0]
  let to   = a:args[1]
  if from ==# to
    return a:expr
  endif

  let esc_verymagic     = (from ==# '\v' ? s:non_escaped_backslash : s:escaped_backslash)
  let esc_non_verymagic = (from ==# '\v' ? s:escaped_backslash : s:non_escaped_backslash)

  let vm_m    = '{%?|+&@)'
  let vm_nom  = '{%?|+&@).*~'
  let vm_vnom = '{%?|+&@).*~'
  let type    = join(sort([from, to]), '')
  if !(  type ==# '\m\v'
    \ || type ==# '\M\v'
    \ || type ==# '\V\v'
    \)
    " error
    echoerr "args must include '\\v' and ('\\m' or '\\M' or '\\V')".type
  endif
  let escaped_text = (type ==# '\m\v' ? vm_m : (type ==# '\M\v' ? vm_nom : vm_vnom))

  " この順で呼ばないと駄目
  let res = ' ' . a:expr
  let res = substitute(res, '\%(@<\?\)\@<!'.'=', '\\=', 'g')                 " 前に@か@<がつかない=をエスケープ
  let res = substitute(res, '\([^%]\)'.'\ze[(>]', '\1\\', 'g')               " 前に%がつかない[(>]をエスケープ
  let res = substitute(res, '\([^@%]\)'.'\ze<', '\1\\', 'g')                 " 前に[@%]がつかない<をエスケープ
  let res = substitute(res, esc_verymagic.'\%(@<\?\)'.'\zs=', '\\=', 'g')    " 前に特殊文字として扱われない@か@<がつく=をエスケープ
  let res = substitute(res, esc_verymagic.'\zs%\ze[(>]', '%\\', 'g')      " 前に特殊文字として扱われない%がつく[(>]をエスケープ
  let res = substitute(res, esc_verymagic.'\zs\([@%]\)<', '\1\\<', 'g')      " 前に特殊文字として扱われない[@%]がつく<をエスケープ
  if type ==# '\M\v' || type ==# '\V\v'
    let res = substitute(res, esc_verymagic.'\zs\%(\\_\)\@<!\[', '\\[', 'g') " 前に\_がつかない[をエスケープ
  endif
  if type ==# '\V\v'
    let res = substitute(res, '\%('.esc_non_verymagic.'%\)\@<!'.'\([$^]\)', '\\\1', 'g') " 前に%がつかない$^をエスケープ
  endif
  let res = substitute(res, '\(['.escaped_text.']\)', '\\\1', 'g') " エスケープが必要な記号をエスケープ
  let res = substitute(res, s:escaped_backslash.'\zs\\\\\ze['.escaped_text . (type ==# '\M\v' ? '[' : (type ==# '\V\v' ? '[$^' : '')) . '<>(=]', '', 'g') " 二重エスケープになったものは元々エスケープされていたものなのでアンエスケープ

  return strpart(res, 1)
endfunction
"}}}

" verymagic以外の形式のパターン同士のパターンを相互変換する
function! s:convert_except4verymagic(expr, args) "{{{2
  let from = a:args[0]
  let to   = a:args[1]
  if from ==# to
    return a:expr
  endif

  let m_nom    = '.*~'
  let m_vnom   = '.*~'
  let nom_vnom = ''
  let type    = join(sort([from, to]), '')
  if !(  type ==# '\M\m'
    \ || type ==# '\V\m'
    \ || type ==# '\M\V'
    \)
    " error
    echoerr "args must include pair of '\\m' or '\\M' or '\\V' ".type
  endif

  let escaped_text = (type == '\M\m' ? m_nom : (type == '\V\m' ? m_vnom : nom_vnom))
  let res = ' ' . a:expr
  if type ==# '\M\m' || type ==# '\V\m'
    let res = substitute(res, '\%('.s:escaped_backslash.'\\_\)\@<!\[', '\\[', 'g') " 前に\_がつかない[をエスケープ
  endif
  if type ==# '\V\m' || type ==# '\M\V'
    let res = substitute(res, '\%('.s:non_escaped_backslash.'%\)\@<!'.'\([$^]\)', '\\\1', 'g') " 前に%がつかない$^をエスケープ
  endif
  if type !=# '\M\V'
    let res = substitute(res, '\(['.escaped_text.']\)', '\\\1', 'g') " エスケープが必要な記号をエスケープ
  endif
  let res = substitute(res, s:escaped_backslash.'\zs\\\\\ze['.escaped_text . (type ==# '\M\m' ? '[' : (type ==# '\V\m' ? '[$^' : '$^')) .']', '', 'g') " 二重エスケープになったものは元々エスケープされていたものなのでアンエスケープ

  return strpart(res, 1)
endfunction

"}}}

"}}}

" __END__  "{{{1
" vim: foldmethod=marker
