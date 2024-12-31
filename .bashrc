# unlimited bash history
unset HISTSIZE HISTFILESIZE

e () {
    emacsclient --alternate-editor='' --create-frame  "${@}"
}

tab1080 () {
    swaymsg input 9580:110:PenTablet_Pen  map_to_region 950 200 1000 562
}

tablap () {
    swaymsg input 9580:110:PenTablet_Pen map_to_region 0 0 1280 720
}

gitpushall () {
    find ~ -name '.git' -type d -print0 | xargs --verbose -0I{} git -C {} push
}

export LEDGER_FILE=~/Documents/hledger/yc.hledger

