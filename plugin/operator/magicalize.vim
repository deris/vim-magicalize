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

if exists('g:loaded_operator_magicalize')
  finish
endif
" Saving 'cpoptions' {{{
let s:save_cpo = &cpo
set cpo&vim
" }}}



call operator#user#define('magicalize',       'operator#magicalize#magicalize')
call operator#user#define('verymagicalize',   'operator#magicalize#verymagicalize')
call operator#user#define('nomagicalize',     'operator#magicalize#nomagicalize')
call operator#user#define('verynomagicalize', 'operator#magicalize#verynomagicalize')



let g:loaded_operator_magicalize = 1
" Restore 'cpoptions' {{{
let &cpo = s:save_cpo
" }}}

" __END__
" vim: foldmethod=marker