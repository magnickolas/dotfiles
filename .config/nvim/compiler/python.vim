if exists("current_compiler")
	finish
endif
let current_compiler = "python"

let s:cpo_save = &cpo
set cpo&vim

if exists(":CompilerSet") != 2
	command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet makeprg=python3\ \%:S

let &cpo = s:cpo_save
unlet s:cpo_save
