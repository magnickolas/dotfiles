if exists("current_compiler")
	finish
endif
let current_compiler = "c"

let s:cpo_save = &cpo
set cpo&vim

if exists(":CompilerSet") != 2
	command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet makeprg=gcc\ -O0\ -std=c11\ -Wall\ -Wextra\ -g\ -o\ \%:r\ \%:p

let &cpo = s:cpo_save
unlet s:cpo_save
