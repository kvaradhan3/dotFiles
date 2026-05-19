#! /bin/zsh

# shellcheck source=/Users/kvaradhan/.zinit
. "$HOME"/.zinit
for i in "$ZDOTDIR_COMMON"/*_zlogin_* "$ZDOTDIR_PLATFORM"/*_zlogin_*; do
    if [ -f "$i" ] ; then
        START="$SECONDS"
        . "$i"
        prtiming "$(basename "$i")"	"$START"
    fi
done
