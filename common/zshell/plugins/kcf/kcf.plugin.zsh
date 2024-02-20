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

if [[ ${zsh_loaded_plugins[-1]} != */kcf ]] && \
       [[ -z ${fpath[(r)${0:h}/functions]} ]]
then
    fpath+=( "${0:h}/functions" )
fi

# plugin unload https://github.com/agkozak/zsh-z/blob/16fba5e9d5c4b650358d65e07609dda4947f97e8/zsh-z.plugin.zsh#L680-L698

local deviceHasBattery=false
case "$(uname -s)" in
    Darwin)
        if ioreg -i | grep "AppleSmartBattery " >/dev/null
	then
	    deviceHasBattery=true
	fi
        ;;
    Linux)
        if [[ -d /sys/class/power_supply/BAT0 ]]
	then
	    deviceHasBattery=true
	fi
        ;;
esac

if $deviceHasBattery
then
    # if we were run multiple times, remove earlier instances
    # and set the bcharge again.
    #
    autoload -Uz _bcharge
    export RPROMPT='$(_bcharge) '"${RPROMPT/'$(_bcharge) '/}"
fi
