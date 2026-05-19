#! /bin/zsh

# shellcheck source=/Users/kvaradhan/.zinit
. "$HOME"/.zinit
for i in "$ZDOTDIR_COMMON"/*_zprofile_* "$ZDOTDIR_PLATFORM"/*_zprofile_*; do
    if [ -f "$i" ] ; then
        START="$SECONDS"
        . "$i"
        prtiming "$(basename "$i")"	"$START"
    fi
done
