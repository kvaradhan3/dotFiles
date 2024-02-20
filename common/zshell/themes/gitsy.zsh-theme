# derived from Robby Russell's and minimal theme
#
ZSH_THEME_GIT_PROMPT_PREFIX='%F{blue}%B[%b%F{red}'
ZSH_THEME_GIT_PROMPT_SUFFIX='%F{blue}%B]%b%f'
ZSH_THEME_GIT_PROMPT_DIRTY='%F{yellow}✗%f'
ZSH_THEME_GIT_PROMPT_CLEAN=""
# ✗
# ●
# »

ZSH_THEME_SVN_PROMPT_PREFIX=$ZSH_THEME_GIT_PROMPT_PREFIX
ZSH_THEME_SVN_PROMPT_SUFFIX=$ZSH_THEME_GIT_PROMPT_SUFFIX
ZSH_THEME_SVN_PROMPT_DIRTY=$ZSH_THEME_GIT_PROMPT_DIRTY
ZSH_THEME_SVN_PROMPT_CLEAN=$ZSH_THEME_GIT_PROMPT_CLEAN
ZSH_THEME_HG_PROMPT_PREFIX=$ZSH_THEME_GIT_PROMPT_PREFIX
ZSH_THEME_HG_PROMPT_SUFFIX=$ZSH_THEME_GIT_PROMPT_SUFFIX
ZSH_THEME_HG_PROMPT_DIRTY=$ZSH_THEME_GIT_PROMPT_DIRTY
ZSH_THEME_HG_PROMPT_CLEAN=$ZSH_THEME_GIT_PROMPT_CLEAN

vcs_status() {
    if [[ $(whence in_svn) != "" ]] && in_svn; then
        svn_prompt_info
    elif [[ $(whence in_hg) != "" ]] && in_hg; then
        hg_prompt_info
    else
        git_prompt_info
    fi
}

prompt() {
    # collect and print status before it is mucked up.

    # 27a4  ➤
    # 226B  ≫
    # 22D9  ⋙
    # 27EB  ⟫
    # 2992  ⦒
    # 27eb  ⟫
    # 00bb  »
    #
    _Status='%B%(?+%F{green}+%F{red})\u2992%f%b'     

    # if coming via ssh, print hostname of client
    test "x$SSH_CLIENT" != 'x' && _Hostnm='(%F{cyan}%m%f%) ' || _Hostnm=''

    # basename($cwd)
    _CWkDir='%F{cyan}%1~%f'

    _v=$(vcs_status)
    _p="${_Hostnm}${_CWkDir}${_v}${_Status}"
    echo "%(!.%K{191}${_p}%k.${_p})"
}

# function prompt-length() {
#   emulate -L zsh
#   local COLUMNS=${2:-$COLUMNS}
#   local -i x y=${#1} m
#   if (( y )); then
#     while (( ${${(%):-$1%$y(l.1.0)}[-1]} )); do
#       x=y
#       (( y *= 2 ))
#     done
#     while (( y > x + 1 )); do
#       (( m = x + (y - x) / 2 ))
#       (( ${${(%):-$1%$m(l.x.y)}[-1]} = m ))
#     done
#   fi
#   echo $x
# }

PROMPT='$(prompt)'
RPROMPT="${RPROMPT/'%*'}"'%*'
