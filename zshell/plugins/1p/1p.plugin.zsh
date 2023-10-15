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

if [[ ${zsh_loaded_plugins[-1]} != */1p ]] && \
       [[ -z ${fpath[(r)${0:h}/functions]} ]]
then
    fpath+=( "${0:h}/functions" )
fi

#
# SC3044: In POSIX sh, disown is undefined.
#
# Requires tidy-viewer, jq,
#
local FQDN=".1password.com"
local CLIPBOARD_TIMEOUT=60	# keep pw in clipboard for this amt of time.

typeset -A __1P
__1P[FUNCTIONS]=''
__1P[UTILITIES]=''

case $(uname) in
Linux)   __1P[copy]=wl-copy ;;
Darwin)  __1P[copy]=pbcopy  ;;
esac

1p_plugin_unload() {
    local x
    for x in ${=__1P[FUNCTIONS]} ${=__1P[UTILITIES]}
    do
        whence -w $x &>/dev/null && unfunction $x
    done
    unset __1P

    fpath=("${(@)fpath:#${0:A:h}}")
    unfunction $0
}

autoload -Uz 1p
