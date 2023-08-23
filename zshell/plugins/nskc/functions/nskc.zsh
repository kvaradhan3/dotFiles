#! /bin/zsh
#
# syntax: nskc cmd [prod] [cluster]
# nskc list ...
# nskc kubeconfig ...
#

#function _nskc {
  0=${(%):-%x}

  usage='usage: nskc {list|config} [prod] [cluster...]' >&2
  if [[ $# == 0 ]]; then
      cmd=help
  else
      cmd=$1 ; shift
  fi
  if [ "$1" = "prod" ] ; then
      export NSK_PROFILE=prod
      shift
  fi
  case $cmd in
  l|ls|list)
    if [ $# -eq 0 ] ; then
      nsk cluster list
    else
      for i in "$@" ; do
        nsk cluster list -n "$i"
      done
    fi | sed 's/  */,/g' | cut -d, -f1-4
    ;;
  c|kc|config|kubeconfig)
    if [ $# -eq 0 ] ; then
      set -- `kcf`
    fi
    for i in "$@" ; do 
      nsk cluster kubeconfig -n "$i" --dest "$i".yaml
      kcf -a -F "$i".yaml 
      rm -f "$i".yaml
    done
    ;;
  *)
    echo $usage >&2
    echo "    Abbreviations: l|ls|list  c|kc|config|kukbeconfig" >&2
    return 2
    ;;
  esac
#}
