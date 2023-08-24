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
    _Status='%B%(?+%F{green}+%F{red})➜%f%b'
    _CWkDir='%F{cyan}%1~%f '
    test "x$SSH_CLIENT" != 'x' && _Hostnm='(%F{cyan}%m%f%) ' || _Hostnm=
    _Prompt="${_Host} ${_CWkDir}"'$(vcs_status)'"${_Status}"
    _v=$(vcs_status)
    _p="${_Hostnm}${_CWkDir}${_v}${_Status}"
    echo "%(!.%K{191}${_p}%k.${_p})"
}

PROMPT='$(prompt) '
RPROMPT='%*'
