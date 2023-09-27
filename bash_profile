[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

alias e='emacsclient -t'
alias ec='emacsclient -c -n'

export AWS_DEFAULT_REGION=ap-southeast-2

 # add idp https://community.rea-group.com/people/steve.occhipinti/blog/2017/01/03/making-aws-authentication-nicer
 # function idp {
 #   export $(rea-as saml | pecorb | xargs rea-as saml) > /dev/null
 #   echo "Role: ${AWS_ROLE-(not set)}"
# }

 function okta {
   export $(rea-as okta | peco | xargs rea-as okta) > /dev/null
   echo "Role: ${AWS_ROLE-(not set)}"
 }

export REA_AS_MFA_METHOD='OKTA_PUSH'

export EDITOR='emacsclient -t'

# colors!
green="\[\033[0;32m\]"
blue="\[\033[0;34m\]"
purple="\[\033[0;35m\]"
reset="\[\033[0m\]"

export GIT_PS1_SHOWDIRTYSTATE=1
# '\u' adds the name of the current user to the prompt
nv# '\$(__git_ps1)' adds git-related stuff
# '\W' adds the name of the current directory
export PS1="$purple$([[ -n '$AWS_ROLE' ]] && echo '$AWS_ROLE')$green\$(__git_ps1)$blue \W $ $reset"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
eval "$(lua /usr/local/bin/z.lua --init bash enhanced once echo fzf)"
alias zf='z -I'

eval "$(rbenv init -)"


# Authenticate to cowbell docker registry

function authenticate_docker() {
  aws s3 cp s3://rea-slips-ap-southeast-2/${AWS_ACCOUNT}/${AWS_ROLE}/registry.cowbell.realestate.com.au - \
   | (IFS=: read u p; docker login -u "$u" -p "$p" registry.cowbell.realestate.com.au)
}

export PATH="$PATH:$HOME/.bash-my-aws/bin"
source ~/.bash-my-aws/aliases
source ~/.aws-shortcuts/aws-shortcuts.sh

alias console='open "$(aws-console-url)"'

export BASH_SILENCE_DEPRECATION_WARNING=1

export GIT_DUET_ROTATE_AUTHOR=1 # To rotate author and co-author roles
export GIT_DUET_CO_AUTHORED_BY=1  # To use co-authored instead of signed-off-by


source "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.bash.inc"
source "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.bash.inc"
export PATH="/usr/local/opt/openjdk@11/bin:$PATH"
export JAVA_HOME=$(/usr/libexec/java_home)

export GITHUB_API_TOKEN="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"
export BUILDKITE_API_TOKEN="aa200590679e49169ec12981de09f8f6f78a5a41"


alias bku="docker run --rm -v \"\${PWD}:/cwd\" -w /cwd -e BUILDKITE_API_TOKEN -e GITHUB_API_TOKEN 639347700193.dkr.ecr.ap-southeast-2.amazonaws.com/rea/buildkite-utils"


export NVM_DIR="$HOME/.nvm"
  [ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
  [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
source "$HOME/.rea-cli/rea-shell-init.sh"

export TDD_PROJECT_ROOT=/Users/carlos.rojas/Developer/rea/coding_dojo/tdd-project

export GO111MODULE="on"
export GOPATH=""

export PATH=$HOME/go/bin:$PATH
export PATH="$PATH:/usr/local/smlnj/bin"
export PATH=/Users/carlos.rojas/go/bin:/Users/carlos.rojas/.rea-cli/bin:/Users/carlos.rojas/.nvm/versions/node/v16.18.0/bin:/usr/local/opt/openjdk@11/bin:/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin:/Users/carlos.rojas/.rbenv/shims:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/go/bin:/opt/X11/bin:/usr/local/opt/fzf/bin:/Users/carlos.rojas/.bash-my-aws/bin:/usr/local/smlnj/bin:~/Desktop/nand2tetris/tools
