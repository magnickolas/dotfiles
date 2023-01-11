#!/bin/sh
percent=$(df / | sed -n "s/.*[^[:digit:]]\+\([[:digit:]]\+\)%.*/\1/p")
printf '<fn=3>﫭</fn> %s%%' "${percent}"
