# when HISTTIMEFORMAT is set;
# save timestamp (unix epoch) in history file;
# and display history in this format when $(history) is invoked
export HISTTIMEFORMAT="%m-%dT%H:%M "
# longer shell history than default (500)
export HISTFILESIZE=10000
export HISTSIZE=10000
# safe shell options
# see bash man page for details
set -o posix
set -o noglob
set -o nounset
set -o noclobber

e () {
    emacsclient --alternate-editor='' --create-frame  "${@}"
}

tab1080 () {
    swaymsg input 9580:110:PenTablet_Pen  map_to_region 950 200 1000 562
}

tablap () {
    swaymsg input 9580:110:PenTablet_Pen map_to_region 0 0 1280 720
}

web () {
    hugo --source $HOME/Web serve
}

gitpushall () {
    find ~ -name '.git' -type d -print0 | xargs --verbose -0I{} git -C {}/.. push
}

gitstatusall () {
    find ~ -name '.git' -type d -print0 | xargs --verbose -0I{} git -C {}/.. status
}

export LEDGER_FILE=$HOME/Documents/hledger/yc.hledger

ede () {
    name=${@}
    espeak -s 170 -v mb/mb-de6 -m -w ${name}.wav -f ${name};
    opusenc --speech --bitrate 32 ${name}.wav ${name}.opus
    rm ${name}.wav
}

connfiio () {
    fiiomac="40:ED:98:19:8E:39"
    bluetoothctl <<EOF
pair $fiiomac
connect $fiiomac
EOF
}

# w3m needs history file to exist to save browsing history
if ! [ -e $HOME/.w3m/history ]; then touch $HOME/.w3m/history; fi
