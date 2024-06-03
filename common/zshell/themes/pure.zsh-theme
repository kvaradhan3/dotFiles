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

autoload -U promptinit; promptinit

# optionally define some options
PURE_CMD_MAX_EXEC_TIME=10

# change the path color
zstyle :prompt:pure:path color white

# change the color for both `prompt:success` and `prompt:error`
zstyle ':prompt:pure:prompt:*' color cyan

# turn on git stash status
zstyle :prompt:pure:git:stash show yes

prompt pure


RPROMPT="${RPROMPT/'%*'}"'%*'
