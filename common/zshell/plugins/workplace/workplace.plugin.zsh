#! /bin/zsh
# shellcheck disable=SC2086,SC2278,SC2298,SC2296,SC2299
# SC2086 (info): Double quote to prevent globbing and word splitting.
# SC2296 (error): Parameter expansions can't start with (. Double check syntax.
# SC2298 (error): ${${x}} is invalid. For expansion, use ${x}.
#        For indirection, use arrays, ${!x} or (for sh) eval.
# SC2278 (error): $0 can't be assigned in Ksh (but it does reflect the current function).
# SC2299 (error): Parameter expansions can't be nested. Use temporary variables.
#

0="${ZERO:-${${0:#$ZSH_ARGZERO}:-${(%):-%N}}}"
0="${${(M)0:#/*}:-$PWD/$0}"

if [[ ${zsh_loaded_plugins[-1]} != */workplace ]] && \
       [[ -z ${fpath[(r)${0:h}/functions]} ]]
then
    fpath+=( "${0:h}/functions" )
fi

local flist="${0:h}/functions"

workplace_plugin_unload() {
    local _i
    for _i in $flist/*
    do
        whence -w ${_i#$flist/} &>/dev/null && unfunction ${_i#$flist/}
    done
    whence -w work_plugin_unload &>/dev/null && work_plugin_unload

    fpath=("${(@)fpath:#${0:A:h}}")
    unfunction $0
}

autoload -Uz jf		# jira format definer
autoload -Uz spr	# helper to generate github PR reference
autoload -Uz work
autoload -Uz tsw
