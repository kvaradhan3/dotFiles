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

if [[ ${zsh_loaded_plugins[-1]} != */batteryCharge ]] && \
       [[ -z ${fpath[(r)${0:h}/functions]} ]]
then
    fpath+=( "${0:h}/functions" )
fi

# plugin unload https://github.com/agkozak/zsh-z/blob/16fba5e9d5c4b650358d65e07609dda4947f97e8/zsh-z.plugin.zsh#L680-L698

autoload -Uz _msg
autoload -Uz error
autoload -Uz info
autoload -Uz log
autoload -Uz usage
