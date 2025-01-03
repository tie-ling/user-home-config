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

export LEDGER_FILE=$HOME/Documents/hledger/yc.hledger

ede () {
    espeak -s 250 -v mb/mb-de6 -m -f ${@}
}
