# https://old.reddit.com/r/termux/comments/1k6k3se/how_use_termuxservices_and_sshagent_in_01182/mor8aet/
export SSH_AUTH_SOCK="$PREFIX"/var/run/ssh-agent.socket

# Use longer shell history
export HISTFILESIZE=10000
export HISTSIZE=10000

rundictd () {
    dictd --pid-file $PREFIX/var/run/dictd.pid -s -c ~/.config/dictd.conf --locale en_US.UTF-8
}

# save timestamp (unix epoch) in history file
export HISTTIMEFORMAT="%m-%dT%H:%M "

# safe shell options
# see bash man page for details
set -o posix
set -o noglob
set -o nounset
set -o noclobber

if ! [ -e $HOME/.w3m/history ]; then touch $HOME/.w3m/history; fi

e () {
  mg "$@" 
}

export EDITOR=mg

gitpushall () {
    find ~ -maxdepth 2 -name '.git' -type d -print0 | xargs --verbose -0I{} git -C {}/.. push
}

gitpullall () {
    find ~ -maxdepth 2 -name '.git' -type d -print0 | xargs --verbose -0I{} git -C {}/.. pull
}

gitstatusall () {
    find ~ -maxdepth 2 -name '.git' -type d -print0 | xargs --verbose -0I{} git -C {}/.. status
}
