#compdef _foo foo

function _drone() {
	local line

    _arguments -s -w -C \
       "-t[server auth token \[\$DRONE_TOKEN\]]" \
       "--token[server auth token \[\$DRONE_TOKEN\]]" \
       "-s[server address \[\$DRONE_SERVER\]]" \
       "--server[server address \[\$DRONE_SERVER\]]" \
       "--autoscaler[autoscaler address \[\$DRONE_AUTOSCALER]" \
       "-h[show help]" \
       "--help[show help]" \
       "-v[print the version]" \
       "--version[print the version]" \
       "1: :build\:'manage builds'" \
       "*::arg:->args"

    #		"1: :(build cron log encrypt exec info repo secret server queue orgsecret autoscale convert lint sign jsonnet starlark template help h)" \

    case $line[1] in
	build)  _drone_build ;;
    esac
}

function _drone_build() {
  echo manage builds
}

COMMANDS:
   build      manage builds
   cron       manage cron jobs
   log        manage logs
   encrypt    encrypt a secret
   exec       execute a local build
   info       show information about the current user
   repo       manage repositories
   user       manage users
   secret     manage secrets
   server     manage servers
   queue      queue operations
   orgsecret  manage organization secrets
   autoscale  manage autoscaling
   convert    convert legacy format
   lint       lint the yaml file
   sign       sign the yaml file
   jsonnet    generate .drone.yml from jsonnet
   starlark   generate .drone.yml from starlark
   plugins    plugin helper functions
   template   manage templates
   help, h    Shows a list of commands or help for one command
