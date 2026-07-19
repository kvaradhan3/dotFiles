
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

if [[ ${zsh_loaded_plugins[-1]} != */log ]] && \
       [[ -z ${fpath[(r)${0:h}/functions]} ]]
then
    fpath+=( "${0:h}/functions" )
fi

if (( ! $+commands[brew] )); then
  if [[ -n "$BREW_LOCATION" ]]; then
    if [[ ! -x "$BREW_LOCATION" ]]; then
      echo "[oh-my-zsh] $BREW_LOCATION is not executable"
      return
    fi
  elif [[ -x /opt/homebrew/bin/brew ]]; then
    BREW_LOCATION="/opt/homebrew/bin/brew"
  elif [[ -x /usr/local/bin/brew ]]; then
    BREW_LOCATION="/usr/local/bin/brew"
  elif [[ -x /home/linuxbrew/.linuxbrew/bin/brew ]]; then
    BREW_LOCATION="/home/linuxbrew/.linuxbrew/bin/brew"
  elif [[ -x "$HOME/.linuxbrew/bin/brew" ]]; then
    BREW_LOCATION="$HOME/.linuxbrew/bin/brew"
  else
    return
  fi

  # Only add Homebrew installation to PATH, MANPATH, and INFOPATH if brew is
  # not already on the path, to prevent duplicate entries. This aligns with
  # the behavior of the brew installer.sh post-install steps.
  eval "$("$BREW_LOCATION" shellenv)"
fi

if [[ -z "$HOMEBREW_PREFIX" ]]; then
  # Maintain compatibility with potential custom user profiles, where we had
  # previously relied on always sourcing shellenv. OMZ plugins should not rely
  # on this to be defined due to out of order processing.
  export HOMEBREW_PREFIX="$(brew --prefix)"
fi

# Add Homebrew sbin to PATH if it exists and is not already in PATH.
# Homebrew's shellenv only adds bin directories, not sbin. Some formulae
# (e.g. mtr) install executables to sbin, and brew doctor warns if it's
# missing from PATH.
if [[ -d "$HOMEBREW_PREFIX/sbin" ]]; then
  if [[ ! "$PATH" == *"$HOMEBREW_PREFIX/sbin"* ]]; then
    export PATH="$HOMEBREW_PREFIX/sbin:$PATH"
  fi
fi

if [[ -d "$HOMEBREW_PREFIX/share/zsh/site-functions" ]]; then
  fpath+=("$HOMEBREW_PREFIX/share/zsh/site-functions")
fi

alias ba='brew autoremove'
alias bcfg='brew config'
alias bclup="brew cleanup"
alias bdr='brew doctor'
alias bi="brew install"
alias bic="brew install --cask"
alias bih="brew install --HEAD"
alias binf="brew info"
alias binfc="brew info --cask"
alias bls="brew list"
alias blsc="brew list --cask"
alias blsp="brew list --pinned"
alias bo="brew outdated"
alias boc="brew outdated --cask"
alias brewp='brew pin'
alias bri="brew reinstall"
alias bric="brew reinstall --cask"
alias brih="brew reinstall --HEAD"
alias bs='brew search'
alias bsvl='brew services list'
alias bsvoff='brew services stop'
alias bsvon='brew services start'
alias bsvrun='brew services run'
alias bupd='brew update'
alias bupg="brew upgrade"
alias buz='brew uninstall --zap'

alias bcubc='brew upgrade --cask && brew cleanup'
alias bcubo='brew update && brew outdated --cask'
alias bubo='brew update && brew outdated'
alias bubu='bubo && bupg'
alias bubug='bubo && bugbc'
alias bugbc='brew upgrade --greedy && brew cleanup'

# maintainence
alias bmt="brew update && brew outdated && brew upgrade --greedy && brew cleanup"

function brews() {
  local blue="$(tput setaf 4)"
  local bold="$(tput bold)"
  local off="$(tput sgr0)"

  echo "${blue}==>${off} ${bold}Formulae${off}"
  brew leaves                                   | \
    xargs brew deps --installed --for-each      | \
    sed "s/^\(.*\):\(.*\)$/\1${blue}\2${off}/"

  echo "\n${blue}==>${off} ${bold}Casks${off}"
  brew list --cask 2>/dev/null
}

unset BREW_LOCATION
