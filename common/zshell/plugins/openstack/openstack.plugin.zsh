# general
alias os='openstack'
function oscl {
  export OS_CLOUD=$@
}
alias -g @J='--format json'
alias -g @Y='--format yaml'
alias -g @C='--format csv'
alias -g @CSV='--format csv'
alias -g @V='--format value'

alias -g @Jp='--format json | jq -C .'
alias -g @Yp='--format yaml | yq -C .'
alias -g @Cp='--format csv  | tv -af '

alias -g @Jq='--format json | jq'
alias -g @Yq='--format yaml | yq'

# compute
alias osc='openstack compute'
alias oscsl='openstack compute service list'

# server
alias oss='openstack server'
alias ossl='openstack server list'
alias osssh='openstack server show'
alias ossta='openstack server start'
alias osstp='openstack server stop'
alias ossr='openstack server reboot'
alias ossd='openstack server delete'
alias ossip='() { openstack server show $@ --format json | jq .addresses ; }'
alias ossc='() {
    local -A _osc_args=( --boot-from-volume 5
                         --key-name flow
                         --flavor   generic.2vcpu.2GBram.20GBdisk.perf
                         --network  EXT_OAM_1_DHCP
                         --image    GenericUbuntuLinuxMinimal22.04
                        )
    local -a _osc_misc=()
    local _osc_help=false

    for __i in "$@" ; do
        if [[ $__i = help ]] || [[ $__i = -h ]]; then
            _osc_help=true
            continue
        fi

        if [[ $__i =~ ".*=.*" ]] ; then
           for __k __v in $(echo $__i | tr "=" " ") ; do
               $_osc_args[$__k]=$__v
            done
        else
            _osc_misc[1,0]="$__i"
        fi
    done
    if ${_osc_help} ; then
        openstack server create -h
        printf "\ndefaults:\n    openstack server create %s %s\n"      \
                "${(kv)_osc_args}" "${(@)_osc_misc}"
    else
        openstack server create ${(kv)_osc_args} ${(@)_osc_misc}
    fi
}'
    
# images
alias osi='openstack image'
alias osil='openstack image list'
alias osis='openstack image show'

# service
alias osvc='openstack service'
alias osvcl='openstack service list'

# port
alias osp='openstack port'
alias ospl='openstack port list'
alias ospsh='openstack port show'
alias ospc='openstack port create'
alias ospd='openstack port delete'
alias osps='openstack port set'
alias ospu='openstack port unset'

# project
alias osj='openstack project'
alias osjc='openstack project create'
alias osjd='openstack project delete'
alias osjl='openstack project list'
alias osjs='openstack project set'
alias osjsh='openstack project show'
alias osjp='openstack project purge'
alias osjcl='openstack project cleanup'

